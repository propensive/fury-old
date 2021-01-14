/*

    Fury, version 0.33.0. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.model._, fury.io._, fury.text._, fury.utils._

import jovian._
import mercator._
import gastronomy._

import ch.epfl.scala.bsp4j.{CompileResult => _, _}

import scala.concurrent._, duration._, ExecutionContext.Implicits.global
import scala.util._
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.HashMap
import scala.collection.JavaConverters._
import scala.annotation.tailrec

import java.net.URI
import java.util.concurrent.{CompletableFuture, ExecutionException, TimeoutException}

object Build {

  private val buildCache: collection.mutable.Map[Path, Future[Try[Build]]] = TrieMap()
  private val requestOrigins: collection.mutable.Map[RequestOriginId, Build] = TrieMap()

  def findBy(ref: ModuleRef): Iterable[Build] = requestOrigins.values.filter(_.target.ref == ref)

  def apply(layer: Layer, dependency: Dependency, layout: Layout, noSecurity: Boolean)
           (implicit log: Log)
           : Try[Build] = for {

    hierarchy <- layer.hierarchy()
    universe  <- hierarchy.universe
    build     <- Build(universe, dependency, layout)
    _         <- Policy.read(log).checkAll(build.requiredPermissions, noSecurity)
    _         <- build.generateFiles()
  } yield build

  def makeTarget(ref: ModuleRef, universe: Universe, layout: Layout)(implicit log: Log): Try[Target] = for {
    project     <- universe(ref.projectId)
    module      <- project(ref.moduleId)
    binaries    <- module.allBinaries.to[List].traverse(_.paths).map(_.flatten)
    checkouts   <- universe.checkout(ref, layout)
    javaVersion <- universe.javaVersion(ref, layout)
    sources     <- module.sources.to[List].traverse(_.dir(checkouts, layout))
    includeSrcs <- module.includes.flatMap(Source.fromInclude(_)).to[List].traverse(_.dir(checkouts, layout))
    workspace   <- universe.workspace(ref, layout).map(_.fold(layout.workspaceDir(WorkspaceId(ref.key)))(_ in layout.baseDir))
  } yield Target(ref, module, workspace, project, checkouts, sources ++ includeSrcs, binaries, javaVersion)

  def apply(universe: Universe, dependency: Dependency, layout: Layout)(implicit log: Log): Try[Build] = {

    def makeGraph(target: Target): Try[Graph] = for {
      modules <- universe.dependencies(dependency.ref, layout)
      targets <- modules.map(_.ref).traverse(makeTarget(_, universe, layout))
    } yield {
      val targetGraph = (targets + target).map { t => t.ref -> t.directDependencies }.toMap
      Graph(targetGraph, targets.map { t => t.ref -> t }.toMap)
    }

    for {
      target      <- makeTarget(dependency.ref, universe, layout)
      graph       <- makeGraph(target)

      targetIndex <- graph.dependencies.keys.filter(_ != ModuleRef.JavaRef).traverse { ref =>
                       makeTarget(ref, universe, layout).map(ref -> _)
                     }
      
      targets      = targetIndex.unzip._2.to[Set]
      snapshot    <- graph.dependencies.keys.filter(_ != ModuleRef.JavaRef).traverse(universe.checkout(_, layout))
      policy       = (if(target.module.kind.needsExec) targets else targets - target).flatMap(_.module.policy)
    } yield {
      val moduleRefToTarget = (targets ++ target.module.compiler().map { d => graph.targets(d.ref) }).map { t => t.ref -> t }.toMap
      val intermediateTargets = targets.filter(_.canAffectBuild)
      
      val subgraphs = Dag(graph.dependencies.mapValues(_.map(_.ref))).subgraph(intermediateTargets.map(_.ref).to[Set] + dependency.ref).connections
      
      Build(target, graph, subgraphs, snapshot.foldLeft(Snapshot())(_ ++ _),
          moduleRefToTarget, targetIndex.toMap, policy.to[Set], universe, layout)
    }
  }

  def asyncBuild(layer: Layer, ref: ModuleRef, layout: Layout)(implicit log: Log): Future[Try[Build]] = {
    def fn: Future[Try[Build]] = Future(Build(layer, Dependency(ref), layout, false))
    buildCache(layout.furyDir) = buildCache.get(layout.furyDir).fold(fn)(_.transformWith(fn.waive))

    buildCache(layout.furyDir)
  }

  def syncBuild(layer: Layer, ref: ModuleRef, layout: Layout, noSecurity: Boolean)
               (implicit log: Log)
               : Try[Build] = {
    val build = Build(layer, Dependency(ref), layout, noSecurity)
    buildCache(layout.furyDir) = Future.successful(build)
    build
  }

  def nextOriginId(build: Build)(implicit log: Log): RequestOriginId = {
    val originId = RequestOriginId.next(log.pid)
    requestOrigins(originId) = build
    originId
  }

  def findOrigin(originId: RequestOriginId): Option[Build] = requestOrigins.get(originId)
}

case class Build(target: Target, 
                 graph: Graph,
                 subgraphs: Map[ModuleRef, Set[ModuleRef]],
                 snapshot: Snapshot,
                 targets: Map[ModuleRef, Target],
                 targetIndex: Map[ModuleRef, Target],
                 requiredPermissions: Set[Permission],
                 universe: Universe,
                 layout: Layout) {

  private[this] val hashes: HashMap[ModuleRef, Digest] = new HashMap()
  lazy val allDependencies: Set[Target] = targets.values.to[Set]

  lazy val deepDependencies: Map[ModuleRef, Set[ModuleRef]] = {
    @tailrec
    def flatten[T](aggregated: Set[T], children: T => Set[T], next: Set[T]): Set[T] =
      if(next.isEmpty) aggregated
      else flatten(aggregated + next.head, children, next - next.head ++ children(next.head))
    
    targetIndex.map { case (targetId, _) =>
      targetId -> flatten[ModuleRef](Set.empty, graph.dependencies(_).map(_.ref).to[Set], Set(targetId))
    }
  }

  def apply(ref: ModuleRef): Try[Target] = targets.get(ref).ascribe(ItemNotFound(ref.moduleId))

  def apply(compiler: CompilerRef): Try[Option[Target]] = compiler match {
    case Javac(_)         => Success(None)
    case BspCompiler(ref) => apply(ref).map(Some(_))
  }

  def checkoutAll()(implicit log: Log): Try[Unit] =
    snapshot.stashes.to[List].traverse(_._2.get(layout)).munit

  def generateFiles()(implicit log: Log): Try[Iterable[Path]] =
    synchronized { Bloop.generateFiles(this) }

  def copyInclude(ref: ModuleRef, include: Include, work: Path)(implicit log: Log): Try[Unit] =
    include.kind match {
      case ClassesDir(dependency) =>
        val dest = include.id.path in work
        ~log.info(msg"Copying classes from $dependency to $dest")
        dest.mkdir().flatMap(layout.classesDir(dependency).copyTo(dest).waive).munit

      case Jarfile(dependency) =>
        val srcs = deepDependencies(dependency).map(layout.classesDir(_))
        ~log.info(msg"Creating JAR file of $dependency at ${include.id.path in work}")
        saveJars(dependency, srcs, include.id.path in work, Some(Args.FatJarArg))
      
      case JsFile(dependency) =>
        val srcs = deepDependencies(dependency).map(layout.classesDir(_))
        ~log.info(msg"Saving JS file of $dependency at ${include.id.path in work}")
        saveJars(dependency, srcs, include.id.path in work, Some(Args.JsArg))

      case FileRef(repoId, path) =>
        val source = repoId match {
          case id: RepoId      => snapshot(id).map(path in _.path)
          case id: WorkspaceId => Success(path in layout.workspaceDir(id))
        }

        source.flatMap { src =>
          val files = src.walkTree.filter(!_.directory).to[List]
          val toCopy = if(files.size > 1) msg"${files.size} files in " else msg""

          log.info(msg"Copying $toCopy${repoId.repo}${':'}$path to ${include.id.path}")

          files.traverse { p =>
            val newDest = p.relativizeTo(src) in (include.id.path in work)
            newDest.mkParents() >>= p.copyTo(newDest).waive
          }
        }.munit
      
      case TarFile(workspace, path) =>
        ~log.info(msg"Tarring $workspace${':'}$path to ${include.id.path}")
      
      case TgzFile(workspace, path) =>
        ~log.info(msg"Tar-gzipping $workspace${':'}$path to ${include.id.path}")
    }

  def classpath(ref: ModuleRef): Set[Path] = ref.javac.fold(Set[Path]()) { ref =>
    requiredTargets(ref).flatMap { target =>
      Set(layout.classesDir(target.ref), layout.resourcesDir(target.ref)) ++ target.binaries
    } ++ targets(ref).binaries
  }

  def persistentOpts(ref: ModuleRef)(implicit log: Log): Try[Set[Provenance[Opt]]] =
    ref.javac.fold(Try(Set[Provenance[Opt]]())) { ref => for {
      target      <- apply(ref)
      compiler    <- apply(target.module.compiler)
      compileOpts <- ~compiler.to[Set].flatMap(_.module.optDefs.filter(_.persistent).map(_.opt(target.module.compiler, Origin.Compiler)))
      refParams   <- ~target.module.opts.filter(_.persistent).map(Provenance(_, target.module.compiler, Origin.Module(target.ref)))
      removals    <- ~refParams.filter(_.value.remove).map(_.value.id)
      inherited   <- target.module.dependencies.to[List].map(_.ref).traverse(persistentOpts(_)).map(_.flatten)

      pOpts       <- ~(if(target.module.kind.is[Plugin]) Set(
                      Provenance(Opt(OptId(str"Xplugin:${layout.classesDir(target.ref)}"), persistent = true,
                          remove = false), target.module.compiler, Origin.Plugin)
                    ) else Set())

    } yield (pOpts ++ compileOpts ++ inherited ++ refParams.filter(!_.value.remove)).filterNot(removals contains
        _.value.id) }

  def aggregatedOpts(ref: ModuleRef)(implicit log: Log): Try[Set[Provenance[Opt]]] =
    ref.javac.fold(Try(Set[Provenance[Opt]]())) { ref => for {
      target    <- apply(ref)
      compiler  <- ~target.module.compiler
      tmpParams <- ~target.module.opts.filter(!_.persistent).map(Provenance(_, compiler, Origin.Local))
      removals  <- ~tmpParams.filter(_.value.remove).map(_.value.id)
      opts      <- persistentOpts(ref)
    } yield (opts ++ tmpParams.filter(!_.value.remove)).filterNot(removals contains _.value.id) }

  def aggregatedOptDefs(ref: ModuleRef): Try[Set[Provenance[OptDef]]] =
    ref.javac.fold(Try(Set[Provenance[OptDef]]())) { ref => for {
      target  <- apply(ref)
      optDefs <- target.module.dependencies.to[List].map(_.ref).traverse(aggregatedOptDefs(_))
    } yield optDefs.flatten.to[Set] ++ target.module.optDefs.map(Provenance(_, target.module.compiler,
        Origin.Module(target.ref))) ++ target.module.compiler().flatMap { dependency =>
        apply(dependency.ref).get.module.optDefs.map(Provenance(_, target.module.compiler, Origin.Compiler)) } }

  def aggregatedPlugins(ref: ModuleRef): Try[Set[Provenance[PluginDef]]] =
    ref.javac.fold(Try(Set[Provenance[PluginDef]]())) { ref => for {
      target           <- apply(ref)
      inheritedPlugins <- target.module.dependencies.to[List].map(_.ref).traverse(aggregatedPlugins(_))
    } yield inheritedPlugins.flatten.to[Set] ++ target.module.kind.as[Plugin].map { plugin =>
        Provenance(PluginDef(plugin.id, ref, plugin.main), target.module.compiler, Origin.Plugin)
    } }
  
  def aggregatedResources(ref: ModuleRef): Try[Set[Source]] =
    ref.javac.fold(Try(Set[Source]())) { ref => for {
      target    <- apply(ref)
      inherited <- target.module.dependencies.to[List].map(_.ref).traverse(aggregatedResources(_))
    } yield inherited.flatten.to[Set] ++ target.module.resources }
  
  def bootClasspath(ref: ModuleRef): Set[Path] = ref.javac.fold(Set[Path]()) { ref =>
    val requiredPlugins = requiredTargets(ref).filter(_.module.kind.is[Plugin]).flatMap { target =>
      Set(layout.classesDir(target.ref), layout.resourcesDir(target.ref)) ++ target.binaries
    }

    val compilerClasspath = targets(ref).module.compiler().flatMap { dependency => classpath(dependency.ref) }
    
    compilerClasspath ++ requiredPlugins
  }

  private def requiredTargets(ref: ModuleRef): Set[Target] =
    ref.javac.fold(Set[Target]()) { ref => deepDependencies(ref).map(targetIndex.apply) }

  def allSources: Set[Path] = targets.values.to[Set].flatMap(_.sourcePaths.to[Set])

  def writePlugin(ref: ModuleRef): Unit = {
    val target = targets(ref)
    if(target.module.kind.is[Plugin]) {
      val file = layout.classesDir(target.ref) / "scalac-plugin.xml"

      target.module.kind.as[Plugin].foreach { plugin =>
        file.writeSync(str"""|<plugin>
                             | <name>${plugin.id}</name>
                             | <classname>${plugin.main}</classname>
                             |</plugin>""".stripMargin)
      }
    }
  }

  def saveNative(ref: ModuleRef, dest: Path, main: ClassRef)(implicit log: Log): Try[Unit] =
    for {
      dest <- Try(dest.extant())
      cp   = runtimeClasspath(ref).to[List].map(_.value)
      _    <- Shell(layout.env).native(dest, cp, main.key)
    } yield ()

  def saveJars(ref: ModuleRef, srcs: Set[Path], dest: Path, out: Option[CliParam])
              (implicit log: Log)
              : Try[Unit] = {
    val bins = allDependencies.flatMap(_.binaries)
    val fatJar = out == Some(Args.FatJarArg)
    val js = out == Some(Args.JsArg)
    val path = dest in layout.baseDir

    def saveJar(staging: Path, module: Module): Try[Unit] = path.mkParents.map { _ =>
      val enc = System.getProperty("file.encoding")
      val manifest = JarManifest(bins.map(_.name), module.kind.as[App].map(_.main.key))
      val jarInputs: Set[Path] = if(fatJar) bins else Set()
      log.info(msg"Saving JAR file ${path.relativizeTo(layout.baseDir)} using ${enc}")
      for {
        resources        <- aggregatedResources(ref)
        _                <- resources.traverse(_.copyTo(snapshot, layout, staging))
        _                <- Shell(layout.env).jar(path, jarInputs, staging.children.map(staging / _).to[Set], manifest)
        _                <- if(!fatJar) bins.traverse { bin => bin.copyTo(path.parent / bin.name) } else Success(())
      } yield {
        if(fatJar) log.info(msg"Wrote ${path.size} to ${path.relativizeTo(layout.baseDir)}")
        else log.info(msg"Wrote ${bins.size + 1} JAR files (total ${bins.foldLeft(ByteSize(0
        ))(_ + _.size)}) to ${path.parent.relativizeTo(layout.baseDir)}")
      }
    }

    def packageJson(bin: Option[String]): euphemism.Json = {
      //TODO move this to fury.utils.ScalaJs
      import euphemism._
      Json.of(
        name = s"${ref.projectId.key}-${ref.moduleId.key}",
        version = "0.0.1",
        bin = bin
      )
    }

    def saveJs(staging: Path, module: Module): Try[Unit] = path.mkParents.map { _ =>
      val enc  = System.getProperty("file.encoding")
      val launcherName = "main.js"
      val json = packageJson(if(module.kind.needsExec) Some(launcherName) else None)
      log.info(msg"Saving Javascript file ${path.relativizeTo(layout.baseDir)} using ${enc}")
      for {
        _ <- ScalaJs.link(module.kind.as[App].map(_.main.key), List(staging) ++ bins, path)
        _ <- (path.parent / "package.json").writeSync(json.toString())
        _ <- if(module.kind.needsExec) saveJsLauncher(launcherName, path.filename) else ~()
      } yield ()
    }

    def saveJsLauncher(launcherName: String, mainFile: String): Try[Unit] = {
      val launcherPath = path.parent / launcherName
      val launcherScript = s"""#!/usr/bin/env node
                              |var launcher = require('./$mainFile');""".stripMargin
      for {
        _ <- launcherPath.writeSync(launcherScript)
        _ <- launcherPath.setExecutable(true)
      } yield ()
    }

    for {
      staging <- aggregateResults(ref, srcs)
      project <- universe(ref.projectId)
      module  <- project(ref.moduleId)
      _       <- if(!js) saveJar(staging, module) else saveJs(staging, module)
      _       <- staging.delete()
    } yield ()
  }

  private[this] def aggregateResults(ref: ModuleRef, compileResults: Set[Path]): Try[Path] = {
    val staging = layout.workDir(ref) / "staging"
    for(_ <- compileResults.filter(_.exists()).traverse(_.copyTo(staging))) yield staging
  }

  def jmhRuntimeClasspath(ref: ModuleRef, classesDirs: Set[Path]): Set[Path] =
    classesDirs ++ targets(ref).module.compiler().map(_.ref).map(layout.resourcesDir(_)) ++ classpath(ref)

  def runtimeClasspath(ref: ModuleRef): Set[Path] =
    targets(ref).module.compiler().flatMap { c => Set(layout.resourcesDir(c.ref), layout.classesDir(c.ref)) } ++
        classpath(ref) + layout.classesDir(ref) + layout.resourcesDir(ref)

  def cleanCache(moduleRef: ModuleRef)(implicit log: Log): Try[CleanCacheResult] = {
    val target = targets(moduleRef)
    val furyTargetIds = deepDependencies(target.ref).toList
    val bspTargetIds = furyTargetIds.map { ref =>
      new BuildTargetIdentifier(str"file://${layout.workDir(ref).value}?id=${ref.urlSafe}")
    }
    val params = new CleanCacheParams(bspTargetIds.asJava)

    BloopServer.borrow(layout.baseDir, this, target.ref, layout) { conn =>
      val result: Try[CleanCacheResult] = wrapServerErrors(conn.server.buildTargetCleanCache(params))
      result.get
    }
  }

  def compileModule(result: BuildResult,
                    target: Target,
                    pipelining: Boolean,
                    globalPolicy: Policy,
                    args: List[String],
                    noSecurity: Boolean)(implicit log: Log)
  : Future[BuildResult] = Future.fromTry {
    val originId = Build.nextOriginId(this)
    val uri: String = str"file://${layout.workDir(target.ref)}?id=${target.ref.urlSafe}"
    val params = new CompileParams(List(new BuildTargetIdentifier(uri)).asJava)
    params.setOriginId(originId.key)
    if(pipelining) params.setArguments(List("--pipeline").asJava)
    val furyTargetIds = deepDependencies(target.ref).toList
    
    val bspTargetIds = furyTargetIds.map { ref =>
      new BuildTargetIdentifier(str"file://${layout.workDir(ref)}?id=${ref.urlSafe}")
    }
    
    val bspToFury = bspTargetIds.zip(furyTargetIds).toMap
    val scalacOptionsParams = new ScalacOptionsParams(bspTargetIds.asJava)

    BloopServer.borrow(layout.baseDir, this, target.ref, layout) { conn =>

      val workDir = if(target.module.includes.nonEmpty) {
        target.module.includes.traverse(copyInclude(target.ref, _, target.workspace))
        Some(target.workspace)
      } else None

      val newResult: Try[BuildResult] = for {
        res  <- wrapServerErrors(conn.server.buildTargetCompile(params))
        opts <- wrapServerErrors(conn.server.buildTargetScalacOptions(scalacOptionsParams))
      } yield BuildResult(res, opts, None)

      val responseOriginId = newResult.get.bspResult.getOriginId
      if(responseOriginId != originId.key) {
        log.note(msg"buildTarget/compile: Expected ${originId.key}, but got $responseOriginId")
      }

      newResult.get.scalacOptions.getItems.asScala.foreach { case soi =>
        val bti = soi.getTarget
        val classDir = soi.getClassDirectory
        val targetId = bspToFury(bti)
        val permanentClassesDir = layout.classesDir(targetId)
        val temporaryClassesDir = Path(new URI(classDir))
        temporaryClassesDir.copyTo(permanentClassesDir)
        //TODO the method setClassDirectory modifies a mutable structure. Consider refactoring
        soi.setClassDirectory(permanentClassesDir.javaFile.toURI.toString)
      }

      (newResult.get, conn.client)
    }.map {
      case (compileResult, client) if compileResult.success && target.module.kind.needsExec =>
        val timeout = target.module.kind.as[App].fold(0)(_.timeout)
        val classDirectories = compileResult.classDirectories
        client.broadcast(StartRun(target.ref))
        
        val future = Future(blocking(run(target, classDirectories.values.to[Set], layout, globalPolicy, args, noSecurity,
            target.javaVersion)))
        val result = Try(Await.result(future, if(timeout == 0) Duration.Inf else Duration(timeout, SECONDS)))
        val exitCode = result.recover { case _: TimeoutException => 124 }.getOrElse(1)
        client.broadcast(StopRun(target.ref))
        compileResult.copy(exitCode = Some(exitCode))
      case (otherResult, _) =>
        otherResult
    }
  }

  private[this] def wrapServerErrors[T](f: => CompletableFuture[T]): Try[T] =
    Try(f.get).recoverWith { case e: ExecutionException => Failure(BuildServerError(e.getCause)) }


  def compile(moduleRef: ModuleRef,
              futures: Map[ModuleRef, Future[BuildResult]] = Map(),
              layout: Layout,
              globalPolicy: Policy,
              args: List[String],
              pipelining: Boolean,
              noSecurity: Boolean)
             (implicit log: Log)
             : Map[ModuleRef, Future[BuildResult]] = {
    val target = targets(moduleRef)

    val newFutures = subgraphs(target.ref).foldLeft(futures) { (futures, dependencyRef) =>
      if(futures.contains(dependencyRef)) futures
      else compile(dependencyRef, futures, layout, globalPolicy, args, pipelining, noSecurity)
    }

    val dependencyFutures = Future.sequence(subgraphs(target.ref).map(newFutures))

    val multiplexer = Lifecycle.currentSession.multiplexer
    multiplexer.start()

    val future: Future[BuildResult] = dependencyFutures.map(BuildResult.merge).flatMap { required =>
      if(!required.success) {
        multiplexer.fire(target.ref, SkipCompile(target.ref))
        multiplexer.close(target.ref)
        Future.successful(required)
      } else {
        val noCompilation = target.sourcePaths.isEmpty && !target.module.kind.needsExec

        if(noCompilation) {
          deepDependencies(target.ref).foreach { ref => multiplexer.fire(ref, NoCompile(ref)) }
          Future.successful(required)
        } else compileModule(required, target, pipelining, globalPolicy, args, noSecurity)
      }
    }

    newFutures.updated(target.ref, future)
  }

  private def run(target: Target, classDirectories: Set[Path], layout: Layout, globalPolicy: Policy,
                  args: List[String], noSecurity: Boolean, javaVersion: Int)
                 (implicit log: Log): Int = {
    val multiplexer = Lifecycle.currentSession.multiplexer
    if (target.module.kind.is[Bench]) {
      classDirectories.foreach { classDirectory =>
        Jmh.instrument(classDirectory, layout.benchmarksDir(target.ref), layout.resourcesDir(target.ref))
        val javaSources = layout.benchmarksDir(target.ref).findChildren(_.endsWith(".java"))

        Shell(layout.env).javac(
          classpath(target.ref).to[List].map(_.value),
          classDirectory.value,
          javaSources.map(_.value).to[List], javaVersion)
      }
    }
    
    val exitCode = Shell(layout.env.copy(workDir = Some(layout.workDir(target.ref).value))).runJava(
      jmhRuntimeClasspath(target.ref, classDirectories).to[List].map(_.value),
      if(target.module.kind.is[Bench]) ClassRef("org.openjdk.jmh.Main")
      else target.module.kind.as[App].fold(ClassRef(""))(_.main),
      securePolicy = target.module.kind.is[App],
      env = target.environment,
      properties = target.properties,
      policy = globalPolicy.forContext(layout, target.ref.projectId),
      args,
      noSecurity,
      layout.workDir(target.ref),
      javaVersion
    ) { ln => multiplexer.fire(target.ref, Print(target.ref, ln)) }.await()

    deepDependencies(target.ref).foreach { ref => multiplexer.fire(ref, NoCompile(ref)) }

    multiplexer.close(target.ref)
    multiplexer.fire(target.ref, StopRun(target.ref))

    exitCode
  }
}