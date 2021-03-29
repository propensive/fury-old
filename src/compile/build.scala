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
import acyclicity._

import ch.epfl.scala.bsp4j.{CompileResult => _, TaskId => _, _}

import scala.concurrent._, duration._, ExecutionContext.Implicits.global
import scala.util._
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.HashMap
import scala.collection.JavaConverters._
import scala.annotation.tailrec

import java.net.URI
import java.util.concurrent.{CompletableFuture, ExecutionException, TimeoutException}
import java.net.URLClassLoader

case class Application(classloader: URLClassLoader, classRef: ClassRef) {
  private lazy val mainMethod = classloader.loadClass(classRef.key).getMethod("main", classOf[Array[String]])
  def apply(args: String*): Unit = mainMethod.invoke(null, args.to[Array])
}

object Build {

  private val buildCache: collection.mutable.Map[Path, Future[Try[Build]]] = TrieMap()

  def apply(layer: Layer, goal: ModuleRef, layout: Layout, noSecurity: Boolean, job: Job): Try[Build] =
    layer.universe().flatMap { universe =>
      val specs: HashMap[ModuleRef, Spec] = HashMap()
    
      def specify(ref: ModuleRef): Try[Spec] = specs.get(ref).map(Success(_)).getOrElse { for {
        project     <- universe(ref.projectId)
        layer       <- universe.layer(project.id)
        module      <- project(ref.moduleId)
        binaries    <- module.allBinaries.to[List].traverse(_.paths).map(_.flatten)
        checkouts   <- universe.checkout(ref, layout)
        stashes     <- ~checkouts.stashes.map { case (k, v) => (v.repoId, k) }.toMap
        javaVersion <- universe.javaVersion(ref, layout)
        workspace   <- universe.workspace(ref, layout).map(_.getOrElse(layout.workspaceDir(project.id, WorkspaceId(ref.key))))
        spec         = Spec(ref, module, workspace, project, layer, checkouts, stashes, binaries, javaVersion)
        _            = specs(ref) = spec
      } yield spec }
      
      for {
        modules   <- universe.dependencies(goal, layout)
        target    <- specify(goal)
        targets   <- modules.map(_.ref).traverse(specify(_))
        // FIXME: Try to remove the use of `Graph`
        graphDeps  = (targets + target).map { t => t.ref -> t.directDependencies }.toMap
        graph      = Graph(graphDeps, targets.map { t => t.ref -> t }.toMap)
        deps       = targets.map { t => t.ref -> t }.toMap.keys.filter(_ != ModuleRef.JavaRef)
        snapshot  <- deps.traverse(universe.checkout(_, layout))
        dag        = Dag(graph.dependencies.mapValues(_.map(_.ref)))
        //build     <- ~Build(goal, dag, target, snapshot.foldLeft(Snapshot())(_ ++ _), universe, layout, specs.toMap, job)
        //_         <- Policy.read().checkAll(build.policy, noSecurity)
        _         <- ~log.info(str"Build contains ${targets.size} targets")
        //_         <- build.generateFiles()
        //_         <- ~Bus.put(ScheduleJob(job, build.execDag.map(_.compileTask)))
      } yield Build(goal, dag, target, snapshot.foldLeft(Snapshot())(_ ++ _), universe, layout, specs.toMap, job)
    }

  def asyncBuild(layer: Layer, ref: ModuleRef, layout: Layout, session: Session): Future[Try[Build]] = {
    val job: Job = Job(msg"Build $ref")
    def fn: Future[Try[Build]] = Future(Build(layer, ref, layout, false, job))
    buildCache(layout.furyDir) = buildCache.get(layout.furyDir).fold(fn)(_.transformWith(fn.waive))

    buildCache(layout.furyDir)
  }

  def syncBuild(layer: Layer, ref: ModuleRef, layout: Layout, noSecurity: Boolean, session: Session): Try[Build] = {
    val job: Job = Job(msg"Build $ref")
    val build = Build(layer, ref, layout, noSecurity, job)
    buildCache(layout.furyDir) = Future.successful(build)
    build
  }
}

case class Graph(dependencies: Map[ModuleRef, Set[Input]], targets: Map[ModuleRef, Spec]) {
  def links: Map[ModuleRef, Set[Input]] = dependencies.map { case (ref, dependencies) =>
    (ref, dependencies.map { dRef => if(targets(dRef.ref).module.kind.is[Compiler]) dRef.hide else dRef })
  }.toMap

  def partialOrder: List[ModuleRef] = sort(dependencies, Nil).reverse
  
  private def sort(todo: Map[ModuleRef, Set[Input]], done: List[ModuleRef]): List[ModuleRef] =
    if(todo.isEmpty) done else {
      val node = todo.find { case (k, v) => (v.map(_.ref) -- done).isEmpty }.get._1
      sort((todo - node).mapValues(_.filter(_.ref != node)), node :: done)
    }

}

case class Spec(ref: ModuleRef, module: Module, workDir: Path, project: Project, layer: Layer,
                snapshot: Snapshot, stashIds: Map[RepoId, StashId], binaries: List[Path], javaVersion: Int) {
  
  def juncture: Boolean = !module.kind.is[Lib] || module.includes.nonEmpty
  def directDependencies = module.dependencies ++ module.compiler()

  // FIXME: This digest should be calculated more carefully
  lazy val hash: Digest = (ref, binaries, stashIds.values.to[List], javaVersion).digest[Md5]
}

case class Build private (goal: ModuleRef, dag: Dag[ModuleRef], spec: Spec, snapshot: Snapshot,
                          universe: Universe, layout: Layout, specs: Map[ModuleRef, Spec], job: Job) { build =>

  val targets: Map[ModuleRef, Target] =
    dag.sorted.foldLeft(Map[ModuleRef, Target]()) { case (acc, ref) => acc.updated(ref, Target(specs(ref))) }
  
  val targetDag: Dag[Target] = dag.map(targets)
  val allDependencies: Set[Target] = targets.values.to[Set]
  //val execDag: Dag[Target] = targetDag.filter { t => t.juncture || t.ref == goal }

  lazy val policy: Set[Permission] =
    (if(spec.module.kind.needsExec) targets else targets - goal).flatMap(_._2.module.policy).to[Set]

  def goalTarget: Target = targets(goal)

  lazy val deepDependencies: Dag[ModuleRef] = dag.closure
  lazy val furyTargetIds = deepDependencies(goal).toList
  lazy val bspTargetIds = furyTargetIds.map { ref => new BuildTargetIdentifier(ref.uri(layout)) }
  lazy val bspToFury: Map[BuildTargetIdentifier, ModuleRef] = bspTargetIds.zip(furyTargetIds).toMap

  def describe: Message = targetDag.sorted.map(_.describe).reduce(_ + "\n\n" + _)

  def apply(ref: ModuleRef): Try[Target] = targets.get(ref).ascribe(ItemNotFound(ref.moduleId))
  def checkoutAll(): Try[Unit] = snapshot.stashes.to[List].traverse(_._2.get(layout)).munit
  
  def apply(compiler: CompilerRef): Try[Option[Target]] = compiler match {
    case Javac(_)         => Success(None)
    case BspCompiler(ref) => apply(ref).map(Some(_))
  }
  
  def compile(session: Session) = for {
    _   <- generateFiles()
    job <- ~job.copy(dag = targetDag.map(_.compileTask))
  } yield Bus.enqueue(msg"Scheduling job $job", _.schedule(session, job).start(job))

  private def apply(input: Input): Try[Target] = apply(input.ref)
  private def generateFiles(): Try[Iterable[Path]] = synchronized { Bloop.generateFiles(this) }

  case class Target(spec: Spec) {
    val module = spec.module
    val ref = spec.ref
    lazy val environment: Map[String, String] = module.environment.map { e => e.id -> e.value }.toMap
    lazy val properties: Map[String, String] = module.properties.map { p => p.id -> p.value }.toMap
    lazy val juncture: Boolean = !module.kind.is[Lib] || module.includes.nonEmpty
    lazy val directDependencies = module.dependencies ++ module.compiler()
    
    lazy val sourcePaths: Try[Set[Path]] =
      module.sources.to[Set].traverse(repoPaths(ref, _)).map(_.flatten)
    
    lazy val editableSourcePaths: Try[Set[Path]] =
      module.sources.filter(_.editable).to[Set].traverse(repoPaths(ref, _)).map(_.flatten)
    
    lazy val compileTask: Task =
      if(juncture || ref == goal) ExecTask(TaskId(ref.urlSafe), spec.hash.encoded[Hex],
        msg"Compile $ref", () => { log.info("do compilation"); BloopServer.borrow(build.layout, job) { conn =>

        log.info(msg"Got connection ${conn.toString} to compile ${ref.uri(layout)}")
        val params = new CompileParams(List(new BuildTargetIdentifier(ref.uri(layout))).asJava)
        params.setOriginId(job.id.key)
        val scalacOptionsParams = new ScalacOptionsParams(bspTargetIds.asJava)
        //if(module.includes.nonEmpty) module.includes.traverse(copyInclude(_))

        val newResult: Try[BuildResult] = for {
          res  <- wrapServerErrors(job, TaskId(ref.urlSafe))(conn.server.buildTargetCompile(params))
          opts <- wrapServerErrors(job, TaskId(ref.urlSafe))(conn.server.buildTargetScalacOptions(scalacOptionsParams))
        } yield BuildResult(res, opts, None)

        log.info("Result: "+newResult.toString.take(100))

        newResult.get.scalacOptions.getItems.asScala.foreach { options =>
          val permanentClasses = layout.classesDir(bspToFury(options.getTarget))
          val temporaryClasses = Path(new URI(options.getClassDirectory))
          temporaryClasses.copyTo(permanentClasses)
          options.setClassDirectory(permanentClasses.javaFile.toURI.toString)
        }

        log.info(msg"Finished subgroup for ${ref}")
        newResult.get
      /*log.info(msg"Inputs for $ref are ${directDependencies.map(_.ref).map(_.msg).foldLeft(msg"")(_ + msg", " + _)}")
      //directDependencies.map(_.ref).map(targets(_)).map(_.compileTask)
      if(juncture) Task(spec.taskId, msg"Compile $ref") {
        Bus.put(LogMessage(msg"Starting subgroup for ${ref}"))
        
        BloopServer.borrow(build, job) { conn =>
          Bus.put(LogMessage(msg"Got connection ${conn.toString} to compile ${ref.uri(layout)}"))
          val params = new CompileParams(List(new BuildTargetIdentifier(ref.uri(layout))).asJava)
          params.setOriginId(spec.taskId.key)
          val scalacOptionsParams = new ScalacOptionsParams(bspTargetIds.asJava)
          if(module.includes.nonEmpty) module.includes.traverse(copyInclude(_))

          val newResult: Try[BuildResult] = for {
            res  <- wrapServerErrors(conn.server.buildTargetCompile(params))
            opts <- wrapServerErrors(conn.server.buildTargetScalacOptions(scalacOptionsParams))
          } yield BuildResult(res, opts, None)

          Bus.put(LogMessage("Result: "+newResult.toString.take(100)))

          newResult.get.scalacOptions.getItems.asScala.foreach { options =>
            val permanentClasses = layout.classesDir(bspToFury(options.getTarget))
            val temporaryClasses = Path(new URI(options.getClassDirectory))
            temporaryClasses.copyTo(permanentClasses)
            options.setClassDirectory(permanentClasses.javaFile.toURI.toString)
          }

          Bus.put(LogMessage(msg"Finished subgroup for ${ref}"))
          newResult.get
        }
      } else Task(spec.taskId, msg"Virtual compile $ref") {
        Bus.put(LogMessage(msg"Virtual task"))
        null
      }*/
    } }) else VirtualTask(TaskId(ref.urlSafe), spec.hash.encoded[Hex], msg"Compile $ref")

    private def repoPaths(ref: ModuleRef, source: Source): Try[List[Path]] = source match {
      case RepoSource(repoId, path, _) =>
        List(snapshot(spec.stashIds(repoId)).map(_.absolutePath(path))).sequence

      case WorkspaceSource(workspaceId, path) => for {
        project   <- universe(ref.projectId)
        layer     <- universe.layer(project.id)
        workspace <- layer.workspaces.findBy(workspaceId)
      } yield List(path in workspace.local.getOrElse(layout.workspaceDir(project.id, workspace.id)))
      
      case LocalSource(path, glob) =>
        Success(List(path in layout.baseDir))
    }

    def describe: Message = List(
      List(msg"$ref ${'('}${module.kind}${')'}:"),
      if(module.dependencies.isEmpty) Nil else List(msg"    Depends on: ${module.dependencies}"),
      List(msg"      Compiler: ${module.compiler} ${module.opts.map { opt => msg"${'-'}$opt" }}"),
      if(module.sources.isEmpty) Nil
      else List(msg"       Sources: ${module.sources} from ${spec.snapshot.stashes.map(_._2)}"),
      if(module.binaries.isEmpty) Nil else List(msg"      Binaries: ${module.binaries}"),
      //if(stashIds.isEmpty) Nil else List(msg"       Stashes: ${stashIds.map { case (k, v) => msg"$k${'@'}$v"}}"),
      if(module.kind.is[App]) List(msg"Work directory: ${spec.workDir.relativizeTo(layout.baseDir)}") else Nil
    ).flatten.reduce(_ + "\n" + _)

    private def copyInclude(include: Include): Try[Unit] =
      include.kind match {
        case ClassesDir(dependency) =>
          val dest = include.id.path in spec.workDir
          ~log.info(msg"Copying classes from $dependency to $dest")
          dest.mkdir().flatMap(layout.classesDir(dependency).copyTo(dest).waive).munit

        case Jarfile(dependency) =>
          val srcs = deepDependencies(dependency).map(layout.classesDir(_))
          ~log.info(msg"Creating JAR file of $dependency at ${include.id.path in spec.workDir}")
          saveJars(spec.layer, srcs, include.id.path in spec.workDir, Some(Args.FatJarArg))
        
        case JsFile(dependency) =>
          val srcs = deepDependencies(dependency).map(layout.classesDir(_))
          ~log.info(msg"Saving JS file of $dependency at ${include.id.path in spec.workDir}")
          saveJars(spec.layer, srcs, include.id.path in spec.workDir, Some(Args.JsArg))

        case FileRef(repoId, path) =>
          val source = repoId match {
            case id: RepoId      => snapshot(spec.stashIds(id)).map(path in _.path)
            case id: WorkspaceId => Success(path in layout.workspaceDir(spec.project.id, id))
          }

          source.flatMap { src =>
            val files = src.walkTree.filter(!_.directory).to[List]
            val toCopy = if(files.size > 1) msg"${files.size} files in " else msg""

            log.info(msg"Copying $toCopy${repoId.repo}${':'}$path to ${include.id.path}")

            files.traverse { p =>
              val newDest = p.relativizeTo(src) in (include.id.path in spec.workDir)
              newDest.mkParents() >>= p.copyTo(newDest).waive
            }
          }.munit
        
        case TarFile(workspace, path) =>
          ~log.info(msg"Tarring $workspace${':'}$path to ${include.id.path}")
        
        case TgzFile(workspace, path) =>
          ~log.info(msg"Tar-gzipping $workspace${':'}$path to ${include.id.path}")
      }
  
    lazy val aggregatedOptDefs: Try[Set[Provenance[OptDef]]] =
      if(ref.isJavac) Try(Set[Provenance[OptDef]]()) else for {
        optDefs <- module.dependencies.to[List].map(_.ref).traverse(targets(_).aggregatedOptDefs)
      } yield optDefs.flatten.to[Set] ++ module.optDefs.map(Provenance(_, module.compiler,
          Origin.Module(ref))) ++ module.compiler().flatMap { dependency =>
          apply(dependency.ref).get.module.optDefs.map(Provenance(_, module.compiler, Origin.Compiler)) }

    lazy val aggregatedOpts: Try[Set[Provenance[Opt]]] =
      if(ref.isJavac) Try(Set[Provenance[Opt]]()) else for {
        compiler  <- ~module.compiler
        tmpParams <- ~module.opts.filter(!_.persistent).map(Provenance(_, compiler, Origin.Local))
        removals  <- ~tmpParams.filter(_.value.remove).map(_.value.id)
        opts      <- persistentOpts
      } yield (opts ++ tmpParams.filter(!_.value.remove)).filterNot(removals contains _.value.id)

    lazy val classpath: Set[Path] = if(ref.isJavac) Set[Path]() else {
      requiredTargets.flatMap { target =>
        Set(layout.classesDir(target.ref), layout.resourcesDir(target.ref)) ++ target.spec.binaries
      } ++ spec.binaries
    }

    lazy val urlClassloader: URLClassLoader = new URLClassLoader(runtimeClasspath.to[Array].map { path =>
      new java.net.URL(if(path.directory) str"file://${path.value}/" else str"file://${path.value}")
    })

    def application(classRef: ClassRef): Application = Application(urlClassloader, classRef)

    private def jmhRuntimeClasspath(classesDirs: Set[Path]): Set[Path] =
      classesDirs ++ module.compiler().map(_.ref).map(layout.resourcesDir(_)) ++ classpath

    lazy val runtimeClasspath: Set[Path] =
      module.compiler().flatMap { c => Set(layout.resourcesDir(c.ref), layout.classesDir(c.ref)) } ++
          classpath + layout.classesDir(ref) + layout.resourcesDir(ref)

    lazy val bootClasspath: Set[Path] = if(ref.isJavac) Set[Path]() else {
      val requiredPlugins = requiredTargets.filter(_.module.kind.is[Plugin]).flatMap { target =>
        Set(layout.classesDir(target.ref), layout.resourcesDir(target.ref)) ++ target.spec.binaries
      }

      val compilerClasspath = for {
        compiler <- module.compiler()
        cTarget   = targets(compiler.ref)
        cp       <- cTarget.classpath
      } yield cp
      
      compilerClasspath ++ requiredPlugins
    }

    private lazy val persistentOpts: Try[Set[Provenance[Opt]]] =
      if(ref.isJavac) Try(Set[Provenance[Opt]]()) else for {
        compiler    <- apply(module.compiler)
        compileOpts <- ~compiler.to[Set].flatMap(_.module.optDefs.filter(_.persistent).map(_.opt(module.compiler, Origin.Compiler)))
        refParams   <- ~module.opts.filter(_.persistent).map(Provenance(_, module.compiler, Origin.Module(ref)))
        removals    <- ~refParams.filter(_.value.remove).map(_.value.id)
        inherited   <- module.dependencies.to[List].map(_.ref).map(targets(_)).traverse(_.persistentOpts).map(_.flatten)
  
        pOpts       <- ~(if(module.kind.is[Plugin]) Set(
                        Provenance(Opt(OptId(str"Xplugin:${layout.classesDir(ref)}"), persistent = true,
                            remove = false), module.compiler, Origin.Plugin)
                      ) else Set())
  
      } yield (pOpts ++ compileOpts ++ inherited ++ refParams.filter(!_.value.remove)).filterNot(removals contains
          _.value.id)

    private lazy val aggregatedResources: Try[Set[Source]] = if(ref.isJavac) Try(Set[Source]()) else for {
      inherited <- module.dependencies.to[List].map(_.ref).map(targets(_)).traverse(_.aggregatedResources)
    } yield inherited.flatten.to[Set] ++ module.resources
    
    private lazy val requiredTargets: Set[Target] =
      if(ref.isJavac) Set[Target]() else deepDependencies(ref).map(targets(_))

    def writePlugin(): Unit = {
      if(module.kind.is[Plugin]) {
        val file = layout.classesDir(ref) / "scalac-plugin.xml"

        module.kind.as[Plugin].foreach { plugin =>
          file.writeSync(str"<plugin><name>${plugin.id}</name><classname>${plugin.main}</classname></plugin>")
        }
      }
    }

    def saveNative(dest: Path, main: ClassRef): Try[Unit] =
      Shell(layout.env).native(dest.extant(), runtimeClasspath.to[List].map(_.value), main.key)

    def saveJars(layer: Layer, srcs: Set[Path], dest: Path, out: Option[CliParam]): Try[Unit] = {
      val bins = allDependencies.flatMap(_.spec.binaries)
      val fatJar = out == Some(Args.FatJarArg)
      val js = out == Some(Args.JsArg)
      val path = dest.in(layout.baseDir)

      def saveJar(staging: Path, module: Module): Try[Unit] = path.mkParents.map { _ =>
        val enc = System.getProperty("file.encoding")
        val manifest = JarManifest(bins.map(_.name), module.kind.as[App].map(_.main.key))
        val jarInputs: Set[Path] = if(fatJar) bins else Set()
        log.info(msg"Saving JAR file ${path.relativizeTo(layout.baseDir)} using ${enc}")
        for {
          resources <- aggregatedResources
          _         <- Shell(layout.env).jar(path, jarInputs, staging.children.map(staging / _).to[Set], manifest)
          _         <- if(!fatJar) bins.traverse { bin => bin.copyTo(path.parent / bin.name) } else Success(())
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
        staging <- targets(ref).aggregateResults(srcs)
        project <- universe(ref.projectId)
        module  <- project(ref.moduleId)
        _       <- if(!js) saveJar(staging, module) else saveJs(staging, module)
        _       <- staging.delete()
      } yield ()
    }

    def cleanCache(): CleanCacheResult = {
      val furyTargetIds = deepDependencies(ref).toList
      val bspTargetIds = furyTargetIds.map { ref => new BuildTargetIdentifier(ref.uri(layout)) }
      val params = new CleanCacheParams(bspTargetIds.asJava)

      BloopServer.borrow(build.layout, job) { conn =>
        wrapServerErrors(job, TaskId("FIXME"))(conn.server.buildTargetCleanCache(params)).get
      }
    }

    // def compile(futures: Map[ModuleRef, Future[BuildResult]] = Map(),
    //             policy: Policy,
    //             args: List[String],
    //             pipelining: Boolean,
    //             noSecurity: Boolean)
    //           : Map[ModuleRef, Future[BuildResult]] = {
    //   log.info(msg"Running compile on $ref with ${futures.toString}")
    //   /*val newFutures = subgraphs(ref).foldLeft(futures) { (futures, dependencyRef) =>
    //     if(futures.contains(dependencyRef)) futures
    //     else {
    //       log.info("Recursing on compile for $ref")
    //       targets(dependencyRef).compile(futures, policy, args, pipelining, noSecurity)
    //     }
    //   }*/

    //   //val dependencyFutures = Future.sequence(subgraphs(ref).map(newFutures))

    //   //val multiplexer = Lifecycle.currentSession.multiplexer
    //   //multiplexer.start()

    //   // val future: Future[BuildResult] = dependencyFutures.map(BuildResult.merge).flatMap { required =>
    //   //   log.info(msg"RUNNING FUTURE FOR $ref")
    //   //   if(!required.success) {
    //   //     //multiplexer.fire(ref, SkipCompile(ref))
    //   //     //multiplexer.close(ref)
    //   //     Future.successful(required)
    //   //   } else compileModule(required, pipelining, policy, args, noSecurity)
    //   // }

    //   //newFutures.updated(ref, future)
    //   Futur
    // }
    
    private def compileModule(result: BuildResult,
                              pipelining: Boolean,
                              policy: Policy,
                              args: List[String],
                              noSecurity: Boolean)
                             : Unit = {
      val params = new CompileParams(List(new BuildTargetIdentifier(ref.uri(layout))).asJava)
      val task = compileTask
      //if(pipelining) params.setArguments(List("--pipeline").asJava)
      //val furyTargetIds = deepDependencies(ref).to[List]
      
      //val bspTargetIds = furyTargetIds.map { ref =>
      //  new BuildTargetIdentifier(str"file://${layout.workDir(ref)}?id=${ref.urlSafe}")
      //}
      
      //val bspToFury: Map[BuildTargetIdentifier, Int] = bspTargetIds.zip(furyTargetIds).toMap
      //val scalacOptionsParams = new ScalacOptionsParams(bspTargetIds.asJava)

      /*BloopServer.borrow(layout.baseDir, build, ref, layout, cancellation) { conn =>

        if(module.includes.nonEmpty) module.includes.traverse(copyInclude(_))

        val newResult: Try[BuildResult] = for {
          res  <- wrapServerErrors(conn.server.buildTargetCompile(params))
          opts <- wrapServerErrors(conn.server.buildTargetScalacOptions(scalacOptionsParams))
        } yield BuildResult(res, opts, None)

        newResult.get.scalacOptions.getItems.asScala.foreach { options =>
          val permanentClasses = layout.classesDir(bspToFury(options.getTarget))
          val temporaryClasses = Path(new URI(options.getClassDirectory))
          temporaryClasses.copyTo(permanentClasses)
          options.setClassDirectory(permanentClasses.javaFile.toURI.toString)
        }

        newResult.get
      }*/
      
      /*task.future.flatMap {
        case Success(compileResult) if compileResult.success && module.kind.needsExec =>
          val timeout = module.kind.as[App].fold(0)(_.timeout)
          val classDirectories = compileResult.classDirectories
          //client.broadcast(StartRun(ref))
          //Bus.put(StartJob(job))
          
          val future = Future(blocking { run(classDirectories.values.to[Set], policy, args, noSecurity) })
          val expiry = Duration(if(timeout == 0) 60 else timeout, SECONDS)
          val result = Try(Await.result(future, expiry))
          val exitCode = result.recover { case _: TimeoutException => 124 }.getOrElse(1)
          
          //client.broadcast(StopRun(ref))
          Future.fromTry(Success(compileResult.copy(exitCode = Some(exitCode))))
        case otherResult =>
          log.info(msg"Compile produced otherResult: ${otherResult.toString}")
          Future.fromTry(otherResult)
      }*/
    }
    
    private def run(classDirectories: Set[Path], policy: Policy, args: List[String], noSecurity: Boolean)
           : Int = {
      //val multiplexer = Lifecycle.currentSession.multiplexer
      if (module.kind.is[Bench]) {
        classDirectories.foreach { classDirectory =>
          Jmh.instrument(classDirectory, layout.benchmarksDir(ref), layout.resourcesDir(ref))
          val javaSources = layout.benchmarksDir(ref).findChildren(_.endsWith(".java"))

          Shell(layout.env).javac(
            classpath.to[List].map(_.value),
            classDirectory.value,
            javaSources.map(_.value).to[List], spec.javaVersion)
        }
      }
      
      val exitCode = Shell(layout.env.copy(workDir = Some(spec.workDir.value))).runJava(
        jmhRuntimeClasspath(classDirectories).to[List].map(_.value),
        if(module.kind.is[Bench]) ClassRef("org.openjdk.jmh.Main")
        else module.kind.as[App].fold(ClassRef(""))(_.main),
        securePolicy = module.kind.is[App],
        env = environment,
        properties = properties,
        policy = policy.forContext(layout, ref.projectId),
        args,
        noSecurity,
        spec.workDir,
        spec.javaVersion
      ) { ln => ()/*multiplexer.fire(ref, Print(ref, ln))*/ }.await()

      deepDependencies(ref).foreach { ref => /*multiplexer.fire(ref, NoCompile(ref))*/() }

      //multiplexer.close(ref)
      //multiplexer.fire(ref, StopRun(ref))
      if(spec.workDir.children.isEmpty) spec.workDir.delete()

      exitCode
    }
  
    private def aggregateResults(compileResults: Set[Path]): Try[Path] = {
      val staging = layout.workDir(ref) / "staging"
      compileResults.filter(_.exists()).traverse(_.copyTo(staging)).map { _ => staging }
    }
  }
  
  def editableSources: Set[Path] = targets.values.to[Set].flatMap(_.editableSourcePaths.getOrElse(Nil))

  private[this] def wrapServerErrors[T](job: Job, taskId: TaskId)(f: => CompletableFuture[T]): Try[T] =
    Try(f.get).recoverWith { case e: ExecutionException =>
      val issue = Issue(e.getCause.getMessage, Halt, None, None)
      Bus.enqueue(msg"Build server crashed: ${e.getCause.toString}", _.addIssues(job.id, taskId, List(issue)))
      Failure(BuildServerError(e.getCause))
    }
}
