/*

    Fury, version 0.16.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import java.io._
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.channels._
import java.util.concurrent.{CompletableFuture, ExecutionException, TimeoutException}

import bloop.launcher.LauncherMain
import bloop.launcher.LauncherStatus._
import ch.epfl.scala.bsp4j.{CompileResult => _, _}
import com.google.gson.{Gson, JsonElement}
import fury.core.UiGraph.CompileIssue
import fury.core.Lifecycle.Session
import fury.io._
import fury.model._
import fury.text._
import fury.utils._
import gastronomy._
import kaleidoscope._
import mercator._
import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.reflect.{ClassTag, classTag}
import scala.util._
import scala.util.control.NonFatal

object Build {
  /*case class Graph(deps: Map[ModuleRef, Set[Dependency]], targets: Map[ModuleRef, Build#Target]) {
    def links: Map[ModuleRef, Set[Dependency]] = dependencies.map { case (ref, dependencies) =>
      (ref, dependencies.map { dRef => if(targets(dRef.ref).module.kind.is[Compiler]) dRef.hide else dRef })
    }.toMap

    lazy val dependencies = deps.updated(ModuleRef.JavaRef, Set()) // FIXME
  }

  private val buildCache: collection.mutable.Map[Path, Future[Try[Build]]] = TrieMap()
  private val requestOrigins: collection.mutable.Map[RequestOriginId, Build] = TrieMap()

  def findBy(ref: ModuleRef): Iterable[Build] = requestOrigins.values.filter(_.ref == ref)

  def apply(layer: Layer, dependency: Dependency, layout: Layout, noSecurity: Boolean)
           (implicit log: Log)
           : Try[Build] = for {

    hierarchy <- layer.hierarchy()
    universe  <- hierarchy.universe
    build     <- ~Build(dependency.ref, universe, layout)
    _         <- Policy.read(log).checkAll(build.policy, noSecurity)
    _         <- build.generateFiles(layout)
  } yield build

  def async(layer: Layer, ref: ModuleRef, layout: Layout)(implicit log: Log): Future[Try[Build]] = {
    def future: Future[Try[Build]] = Future(Build(layer, Dependency(ref), layout, false))
    buildCache(layout.furyDir) = buildCache.get(layout.furyDir).fold(future)(_.transformWith(future.waive))

    buildCache(layout.furyDir)
  }

  def sync(layer: Layer, ref: ModuleRef, layout: Layout, noSecurity: Boolean)(implicit log: Log): Try[Build] = {
    val build = Build(ref, universe, layout, noSecurity)
    buildCache(layout.furyDir) = Future.successful(build)
    build
  }

  def nextOriginId(build: Build)(implicit log: Log): RequestOriginId = {
    val originId = RequestOriginId.next(log.pid)
    requestOrigins(originId) = build
    originId
  }

  def findOrigin(originId: RequestOriginId): Option[Build] = requestOrigins.get(originId)*/

  def apply(ref: ModuleRef, universe: Universe, layout: Layout): Build = {

    new Build(ref, universe, layout)

    /*val target: Target = Target(ref)
    val graph: Build.Graph = mkGraph(target)
    
    val targetIndex = graph.dependencies.keys.filter(_ != ModuleRef.JavaRef).traverse { ref =>
      Target(ref).map(ref -> _)
    }.map(_.toMap)

    val targets = targetIndex.map(_.unzip._2.to[Set])
    val checkoutsX = graph.flatMap(_.dependencies.keys.filter(_ != ModuleRef.JavaRef).traverse(universe.checkout(_, layout)))
    val checkouts: Checkouts = checkoutsX.foldLeft(Checkouts(Set()))(_ ++ _)
    val policy: Set[Permission] = (if(target.module.kind.needsExec) targets else targets - target).flatMap(_.module.policy)

    val moduleRefToTarget = (targets ++ target.module.compiler().map { d => graph.targets(d.ref) }).map { t => t.ref -> t }.toMap
    val intermediateTargets = targets.filter(canAffectBuild)

    val subgraphs: Map[ModuleRef, Set[ModuleRef]] = Dag(graph.dependencies.mapValues(_.map(_.ref))).subgraph(intermediateTargets.map(_.ref).to[Set] + dependency.ref).connections

    def directDependencies(target: Target): Set[Dependency] = target.module.dependencies ++ target.module.compiler()
    def canAffectBuild(target: Target): Boolean = !target.module.kind.is[Lib]

    def mkGraph(target: Target): Try[Build.Graph] = for {
      requiredModules <- universe.dependencies(dependency.ref, layout)
      requiredTargets <- requiredModules.map(_.ref).traverse(Target(_, layout))
    } yield {
      val targetGraph = (requiredTargets + target).map { t => t.ref -> directDependencies(t) }
      Build.Graph(targetGraph.toMap, requiredTargets.map { t => t.ref -> t }.toMap)
    }*/

  }
}

case class Build(ref: ModuleRef, universe: Universe, layout: Layout) {
  private[this] val hashes: HashMap[ModuleRef, Digest] = new HashMap()
  private[this] val targets: HashMap[ModuleRef, Try[Target]] = new HashMap()

  private[this] def target(ref: ModuleRef)(implicit log: Log): Try[Target] =
    targets.getOrElseUpdate(ref, mkTarget(ref))

  def mkTarget(ref: ModuleRef)(implicit log: Log): Try[Target] = for {
    entity    <- universe.entity(ref.projectId)
    module    <- entity.project(ref.moduleId)
    binaries  <- module.allBinaries.to[List].traverse(_.paths).map(_.flatten)
    checkouts <- universe.checkout(ref, layout)
    sources   <- module.sources.to[List].traverse(_.dir(checkouts, layout))
    links     <- module.dependencies.map(_.ref).to[Set].traverse(target(_))
  } yield Target(ref, module, entity, checkouts, sources, binaries, links)
  
  case class Target(ref: ModuleRef,
                    module: Module,
                    entity: Entity,
                    checkouts: Checkouts,
                    sourcePaths: List[Path],
                    binaries: List[Path],
                    dependencies: Set[Target]) {
    lazy val environment: Map[String, String] = module.environment.map { e => e.id -> e.value }.toMap
    lazy val properties: Map[String, String] = module.properties.map { p => p.id -> p.value }.toMap
    lazy val repos: List[Remote] = entity.layer.repos.map(_.remote).to[List]
    
    lazy val deepDependencies: Set[Target] =
      dependencies.filter().traverse(_.deepDependencies).flatten
  }



  //lazy val allDependencies: Set[Target] = targets.values.to[Set]

  /*lazy val deepDependencies: Map[ModuleRef, Set[ModuleRef]] = {
    
    @tailrec
    def flatten[T](aggregated: Set[T], children: T => Set[T], next: Set[T]): Set[T] = {
      if(next.isEmpty) aggregated
      else {
        val node = next.head
        flatten(aggregated + node, children, next - node ++ children(node))
      }
    }
    
    targetIndex.map { case (targetId, _) =>
      targetId -> flatten[ModuleRef](Set.empty, graph.dependencies(_).map(_.ref).to[Set], Set(targetId))
    }
  }*/

  /*def apply(ref: ModuleRef): Try[Target] = targets.get(ref).ascribe(ItemNotFound(ref.moduleId))

  def apply(compiler: CompilerRef): Try[Option[Target]] = compiler match {
    case Javac(_)         => Success(None)
    case BspCompiler(ref) => apply(ref).map(Some(_))
  }*/

  /*def checkoutAll()(implicit log: Log): Try[Unit] = checkouts.checkouts.traverse(_.get(layout)).map(_ => ())
  def generateFiles()(implicit log: Log): Try[Iterable[Path]] = synchronized(Bloop.generateFiles(this, layout))

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
      inherited   <- target.module.dependencies.to[List].map(_.ref).traverse(persistentOpts(_, layout)).map(_.flatten)

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
      opts      <- persistentOpts(ref, layout)
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

    val compilerClasspath = targets(ref).module.compiler().flatMap { dependency => classpath(dependency.ref, layout) }
    
    compilerClasspath ++ requiredPlugins
  }

  private def requiredTargets(ref: ModuleRef): Set[Target] =
    ref.javac.fold(Set[Target]()) { ref => deepDependencies(ref).map(targetIndex.apply) }

  def allSources: Set[Path] = targets.values.to[Set].flatMap(_.sourcePaths.to[Set])

  def writePlugin(ref: ModuleRef, layout: Layout): Unit = {
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

  def saveNative(ref: ModuleRef, dest: Path, layout: Layout, main: ClassRef)(implicit log: Log): Try[Unit] =
    for {
      dest <- Try(dest.extant())
      cp   = runtimeClasspath(ref, layout).to[List].map(_.value)
      _    <- Shell(layout.env).native(dest, cp, main.key)
    } yield ()

  def saveJars(ref: ModuleRef, srcs: Set[Path], destination: Path, layout: Layout, out: Option[CliParam])
              (implicit log: Log)
              : Try[Unit] = {
    val bins = allDependencies.flatMap(_.binaries)
    val fatJar = out == Some(Args.FatJarArg)
    val js = out == Some(Args.JsArg)

    def saveJar(staging: Path, entity: Entity, module: Module): Try[Path] = for {
      manifest  <- ~JarManifest(bins.map(_.name), module.kind.as[App].map(_.main.key))
      dest       = destination.extant()
      path       = (dest / str"${ref.projectId.key}-${ref.moduleId.key}.jar")
      enc        = System.getProperty("file.encoding")
      _          = log.info(msg"Saving JAR file ${path.relativizeTo(layout.baseDir)} using ${enc}")
      resources <- aggregatedResources(ref)
      _         <- resources.traverse(_.copyTo(checkouts, layout, staging))

      _         <- if(!js) Shell(layout.env).jar(path, if(fatJar) bins else Set.empty,
                       staging.children.map(staging / _).to[Set], manifest) else ~()

      _         <- if(!fatJar) bins.traverse { bin => bin.copyTo(dest / bin.name) } else Success(())
      _          = if(fatJar) log.info(msg"Wrote ${path.size} to ${path.relativizeTo(layout.baseDir)}")
                   else log.info(msg"Wrote ${bins.size + 1} JAR files (total ${bins.foldLeft(ByteSize(0
                       ))(_ + _.size)}) to ${path.parent.relativizeTo(layout.baseDir)}")
    } yield path

    for {
      staging <- aggregateResults(ref, srcs, layout)
      entity  <- universe.entity(ref.projectId)
      module  <- entity.project(ref.moduleId)
      path    <- if(!js) saveJar(staging, entity, module) else ~()
      _       <- if(js) ~log.info(msg"Saving JavaScript to ${destination.relativizeTo(layout.baseDir)}") else ~()
      _       <- if(js) ScalaJs.link(module.kind.as[App].map(_.main.key), List(staging) ++ bins, destination) else ~()
      _       <- staging.delete()
    } yield ()
  }

  private[this] def aggregateResults(ref: ModuleRef, compileResults: Set[Path], layout: Layout): Try[Path] = {
    val staging = layout.workDir(ref) / "staging"
    for(_ <- compileResults.filter(_.exists()).traverse(_.copyTo(staging))) yield staging
  }

  def jmhRuntimeClasspath(ref: ModuleRef, classesDirs: Set[Path], layout: Layout): Set[Path] =
    classesDirs ++ targets(ref).module.compiler().map(_.ref).map(layout.resourcesDir(_)) ++ classpath(ref, layout)

  def runtimeClasspath(ref: ModuleRef, layout: Layout): Set[Path] =
    targets(ref).module.compiler().flatMap { c => Set(layout.resourcesDir(c.ref), layout.classesDir(c.ref)) } ++
        classpath(ref, layout) + layout.classesDir(ref) + layout.resourcesDir(ref)

  def cleanCache(moduleRef: ModuleRef, layout: Layout)(implicit log: Log): Try[CleanCacheResult] = {
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

  def compileModule(target: Target,
                    layout: Layout,
                    pipelining: Boolean,
                    globalPolicy: Policy,
                    args: List[String],
                    noSecurity: Boolean)
                   (implicit log: Log)
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
    
    val bspToFury = (bspTargetIds zip furyTargetIds).toMap
    val scalacOptionsParams = new ScalacOptionsParams(bspTargetIds.asJava)

    BloopServer.borrow(layout.baseDir, this, target.ref, layout) { conn =>

      val result: Try[BuildResult] = for {
        res  <- wrapServerErrors(conn.server.buildTargetCompile(params))
        opts <- wrapServerErrors(conn.server.buildTargetScalacOptions(scalacOptionsParams))
      } yield BuildResult(res, opts, None)

      val responseOriginId = result.get.bspResult.getOriginId
      if(responseOriginId != originId.key) {
        log.warn(msg"buildTarget/compile: Expected ${originId.key}, but got $responseOriginId")
      }

      result.get.scalacOptions.getItems.asScala.foreach { case soi =>
        val bti = soi.getTarget
        val classDir = soi.getClassDirectory
        val targetId = bspToFury(bti)
        val permanentClassesDir = layout.classesDir(targetId)
        val temporaryClassesDir = Path(new URI(classDir))
        temporaryClassesDir.copyTo(permanentClassesDir)
        //TODO the method setClassDirectory modifies a mutable structure. Consider refactoring
        soi.setClassDirectory(permanentClassesDir.javaFile.toURI.toString)
      }

      (result.get, conn.client)
    }.map {
      case (compileResult, client) if compileResult.success && target.module.kind.needsExec =>
        val timeout = target.module.kind.as[App].fold(0)(_.timeout)
        val classDirectories = compileResult.classDirectories
        client.broadcast(StartRun(target.ref))
        val future = Future(blocking(run(target, classDirectories, layout, globalPolicy, args, noSecurity)))
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

    val future = dependencyFutures.map(BuildResult.merge).flatMap { required =>
      if(!required.success) {
        multiplexer.fire(target.ref, SkipCompile(target.ref))
        multiplexer.close(target.ref)
        Future.successful(required)
      } else {
        val noCompilation = target.sourcePaths.isEmpty && !target.module.kind.needsExec

        if(noCompilation) {
          deepDependencies(target.ref).foreach { ref =>
            multiplexer.fire(ref, NoCompile(ref))
          }
          Future.successful(required)
        } else compileModule(target, layout, pipelining, globalPolicy, args, noSecurity)
      }
    }

    newFutures.updated(target.ref, future)
  }

  private def run(target: Target,
                  classDirectories: Set[Path],
                  layout: Layout,
                  globalPolicy: Policy,
                  args: List[String],
                  noSecurity: Boolean)
                 (implicit log: Log)
                 : Int = {
    val multiplexer = Lifecycle.currentSession.multiplexer
    if (target.module.kind.is[Bench]) {
      classDirectories.foreach { classDirectory =>
        Jmh.instrument(classDirectory, layout.benchmarksDir(target.ref), layout.resourcesDir(target.ref))
        val javaSources = layout.benchmarksDir(target.ref).findChildren(_.endsWith(".java"))

        Shell(layout.env).javac(
          classpath(target.ref, layout).to[List].map(_.value),
          classDirectory.value,
          javaSources.map(_.value).to[List])
      }
    }
    
    val exitCode = Shell(layout.env).runJava(
      jmhRuntimeClasspath(target.ref, classDirectories, layout).to[List].map(_.value),
      if(target.module.kind.is[Bench]) ClassRef("org.openjdk.jmh.Main")
      else target.module.kind.as[App].fold(ClassRef(""))(_.main),
      securePolicy = target.module.kind.is[App],
      env = target.environment,
      properties = target.properties,
      policy = globalPolicy.forContext(layout, target.ref.projectId),
      layout = layout,
      args,
      noSecurity
    ) { ln => multiplexer.fire(target.ref, Print(target.ref, ln)) }.await()

    deepDependencies(target.ref).foreach { ref => multiplexer.fire(ref, NoCompile(ref)) }

    multiplexer.close(target.ref)
    multiplexer.fire(target.ref, StopRun(target.ref))

    exitCode
  }*/
}