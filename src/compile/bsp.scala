/*

    Fury, version 0.15.1. Copyright 2018-20 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package fury.core

import fury.strings._, fury.io.Path, fury.model._, fury.ogdl._, fury.utils._

import euphemism._

import ch.epfl.scala.bsp4j.{CompileResult => BspCompileResult, _}
import FuryBuildServer._
import gastronomy._
import guillotine._
import mercator._
import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

import java.util.concurrent.{Future => JFuture, CompletableFuture}
import java.io.{InputStream, OutputStream}
import java.net.URI


import language.higherKinds

object Bsp {

  val bspVersion = "2.0.0-M4"

  def createConfig(layout: Layout): Try[Unit] =
    layout.bspConfig.writeSync(bspConfigJson(whichFury(layout)).toString)

  private def whichFury(layout: Layout): Path = Path(System.getProperty("fury.home")) / "bin" / "fury"

  private def bspConfigJson(fury: Path): Json =
    Json.of(
      name = "Fury",
      argv = List(
          fury.javaPath.toAbsolutePath.toString,
          "standalone",
          "bsp"
      ),
      version = FuryVersion.current,
      bspVersion = bspVersion,
      languages = List("java", "scala")
    )

  def startServer(cli: Cli)(implicit log: Log): Try[ExitStatus] =
    for {
      layout  <- cli.layout
      invoc   <- cli.call()
      running <- ~run(System.in, System.out, layout)
    } yield {
      System.err.println("Started bsp process ...")
      running.get()
      Done
    }

  def run(in: InputStream, out: OutputStream, layout: Layout)
         (implicit log: Log): JFuture[Void] = {

    val cancel = new Cancelator()
    val server = new FuryBuildServer(layout, cancel)

    val launcher = new Launcher.Builder[BuildClient]()
      .setRemoteInterface(classOf[BuildClient])
      .setLocalService(server)
      .setInput(in)
      .setOutput(out)
      .create()

    val client = launcher.getRemoteProxy
    server.onConnectWithClient(client)

    val listening = launcher.startListening()
    cancel.cancel = () => listening.cancel(true) // YOLO
    listening
  }

}

class FuryBuildServer(layout: Layout, cancel: Cancelator)(implicit log: Log)
    extends BuildServer with ScalaBuildServer {
  import FuryBuildServer._
  
  private[this] var client: BuildClient = _

  private def structure: Try[Structure] =
    for {
      conf           <- Ogdl.read[FuryConf](layout.confFile, identity(_))
      layer          <- Layer.retrieve(conf)
      hierarchy      <- layer.hierarchy()
      universe       <- hierarchy.universe
      graph          <- layer.projects.flatMap(_.moduleRefs).map { ref =>
                          for {
                            ds   <- universe.dependencies(ref, layout)
                            arts <- (ds + ref).map { d => universe.makeTarget(d, layout) }.sequence
                          } yield arts.map { a =>
                            (a.ref, (a.dependencies.map(_.ref): List[ModuleRef]) ++ (a.compiler
                                .map(_.ref.hide): Option[ModuleRef]))
                          }
                        }.sequence.map(_.flatten.toMap)
      allModuleRefs  = graph.keys
      modules       <- allModuleRefs.traverse { ref => universe.getMod(ref).map((ref, _)) }
      targets       <- graph.keys.map { key =>
                         universe.makeTarget(key, layout).map(key -> _)
                       }.sequence.map(_.toMap)
      checkouts     <- graph.keys.map(universe.checkout(_, layout)).sequence
    } yield Structure(modules.toMap, graph, checkouts.foldLeft(Checkouts(Set()))(_ ++ _), targets)

  private def getCompilation(structure: Structure, bti: BuildTargetIdentifier): Try[Compilation] = {
    for {
      //FIXME remove duplication with structure
      conf        <- Layer.readFuryConf(layout)
      layer       <- Layer.retrieve(conf)
      module      <- structure.moduleRef(bti)
      compilation <- Compilation.syncCompilation(layer, module, layout, noSecurity = false)
    } yield compilation
  }
  
  private[this] var reporter: Reporter = _

  override def onConnectWithClient(buildClient: BuildClient): Unit = {
    client = buildClient
    reporter = new BspReporter(client)
  }

  override def buildInitialize(initializeBuildParams: InitializeBuildParams)
                              : CompletableFuture[InitializeBuildResult] = {
    val capabilities = new BuildServerCapabilities()
    capabilities.setBuildTargetChangedProvider(false)
    val compileProvider = new CompileProvider(List("java", "scala").asJava)
    capabilities.setCompileProvider(compileProvider)
    capabilities.setDependencySourcesProvider(false)
    capabilities.setInverseSourcesProvider(false)
    capabilities.setResourcesProvider(false)
    
    // FIXME: fury can provide run
    val runProvider = new RunProvider(List.empty[String].asJava)
    
    capabilities.setRunProvider(runProvider)
    
    // Fury has no plans to provide a testing interface
    val testProvider = new TestProvider(List.empty[String].asJava)
    
    capabilities.setTestProvider(testProvider)

    val result = new InitializeBuildResult("Fury", FuryVersion.current, Bsp.bspVersion, capabilities)
    val future = new CompletableFuture[InitializeBuildResult]()
    future.complete(result)    
    future
  }

  override def onBuildInitialized(): Unit = {
  }

  override def onBuildExit(): Unit = {
    log.info("**> buildExit")
    cancel.cancel()
    Lifecycle.halt()
  }

  override def buildShutdown(): CompletableFuture[AnyRef] = {
    log.info("**> buildShutdown")
    val result = new CompletableFuture[AnyRef]()
    Lifecycle.shutdown().fold(result.completeExceptionally, result.complete)
    result
  }

  override def workspaceBuildTargets(): CompletableFuture[WorkspaceBuildTargetsResult] = {

    log.info("**> workspaceBuildTargets")

    val result =
      for(struct <- structure)
      yield new WorkspaceBuildTargetsResult(struct.targets.values.map(toTarget(_, struct)).to[List].asJava)

    val future = new CompletableFuture[WorkspaceBuildTargetsResult]()

    result match {
      case Success(value)     => future.complete(value)
      case Failure(exception) => future.completeExceptionally(exception)
    }

    future
  }

  override def buildTargetSources(sourcesParams: SourcesParams): CompletableFuture[SourcesResult] = {

    log.info("**> buildTargetSources")

    val sourceItems = for {
      struct  <- structure
      targets  = sourcesParams.getTargets.asScala
      items   <- targets.traverse { t =>
                   struct.moduleRef(t).map { ref => targetSourcesItem(t, struct.targets(ref)) }
                 }
    } yield new SourcesResult(items.asJava)

    val future = new CompletableFuture[SourcesResult]()
    sourceItems match {
      case Success(value)     => future.complete(value)
      case Failure(exception) => future.completeExceptionally(exception)
    }

    future
  }

  private def targetSourcesItem(target: BuildTargetIdentifier, a: Target): SourcesItem = {
    val items = a.sourcePaths.map { p =>
      new SourceItem(p.javaPath.toUri.toString, SourceItemKind.DIRECTORY, false)
    }.asJava
    
    new SourcesItem(target, items)
  }

  override def buildTargetInverseSources(inverseSourcesParams: InverseSourcesParams)
                                        : CompletableFuture[InverseSourcesResult] = {
    val future = new CompletableFuture[InverseSourcesResult]()
    future.completeExceptionally(new NotImplementedError("method not implemented"))

    future
  }

  override def buildTargetDependencySources(dependencySourcesParams: DependencySourcesParams)
                                           : CompletableFuture[DependencySourcesResult] = {

    log.info("**> buildTargetDependencySources")

    val result = Try(new DependencySourcesResult(List.empty.asJava))
    val future = new CompletableFuture[DependencySourcesResult]()
    
    result match {
      case Success(value)     => future.complete(value)
      case Failure(exception) => future.completeExceptionally(exception)
    }
    
    future
  }

  override def buildTargetResources(resourcesParams: ResourcesParams): CompletableFuture[ResourcesResult] = {
    val future = new CompletableFuture[ResourcesResult]()
    future.complete(new ResourcesResult(Nil.asJava))
    future
  }

  override def buildTargetCompile(compileParams: CompileParams): CompletableFuture[BspCompileResult] = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    val bspTargets = compileParams.getTargets.asScala
    val allResults = bspTargets.traverse { bspTargetId =>
      for {
        globalPolicy <- ~Policy.read(log)
        struct <- structure
        compilation <- getCompilation(struct, bspTargetId)
        moduleRef <- struct.moduleRef(bspTargetId)
      } yield {
        val multiplexer = new Multiplexer[ModuleRef, CompileEvent](compilation.targets.map(_._1).to[Set])
        val session = Lifecycle.currentSession(log)
        session.multiplexer = multiplexer
        
        val compilationTasks = compilation.compile(moduleRef, Map.empty, layout, globalPolicy, List.empty,
            pipelining = false, noSecurity = false)
        
        val aggregatedTask = Future.sequence(compilationTasks.values.toList).map(CompileResult.merge(_))
        aggregatedTask.andThen{case _ => multiplexer.closeAll()}
        reporter.report(compilation.graph, ManagedConfig().theme, multiplexer)
        val synchronousResult = Await.result(aggregatedTask, Duration.Inf)
        synchronousResult
      }
    }
    get(allResults.map{ s =>
      toCompletableFuture(Future.successful {
        CompileResult.merge(s.toList).asBsp
      })
    })
  }

  override def buildTargetTest(testParams: TestParams): CompletableFuture[TestResult] = {
    val future = new CompletableFuture[TestResult]()
    future.completeExceptionally(new NotImplementedError("method not implemented"))
    
    future
  }

  override def buildTargetRun(runParams: RunParams): CompletableFuture[RunResult] = {
    val future = new CompletableFuture[RunResult]()
    future.completeExceptionally(new NotImplementedError("method not implemented"))
    
    future
  }

  override def buildTargetCleanCache(cleanCacheParams: CleanCacheParams)
                                    : CompletableFuture[CleanCacheResult] = {
    //TODO Support cleaning a single module?
    val done = List(layout.bloopDir, layout.analysisDir, layout.classesDir).traverse(_.delete()).isSuccess
    val future = new CompletableFuture[CleanCacheResult]()
    future.complete(new CleanCacheResult("", done))
    future
  }

  private[this] def scalacOptionsItem(target: BuildTargetIdentifier, struct: Structure)
                                     : Try[ScalacOptionsItem] =
    struct.moduleRef(target).map { ref =>
      val art = struct.targets(ref)
      val params = art.params.map(_.parameter)
      val paths = art.binaries.map(_.javaPath.toUri.toString)
      val classesDir = layout.classesDir.javaPath.toAbsolutePath.toUri.toString
      
      new ScalacOptionsItem(target, params.asJava, paths.asJava, classesDir)
    }

  override def buildTargetScalacOptions(scalacOptionsParams: ScalacOptionsParams)
                                       : CompletableFuture[ScalacOptionsResult] = {

    log.info("**> buildTargetScalacOptions")

    val result = for {
      struct  <- structure
      targets  = scalacOptionsParams.getTargets.asScala
      items   <- targets.traverse { target =>
                   struct.moduleRef(target).flatMap { ref => scalacOptionsItem(target, struct) }
                 }
    } yield new ScalacOptionsResult(items.asJava)

    val future = new CompletableFuture[ScalacOptionsResult]()
    
    result match {
      case Success(value)     => future.complete(value)
      case Failure(exception) => future.completeExceptionally(exception)
    }

    future
  }

  override def buildTargetScalaTestClasses(scalaTestClassesParams: ScalaTestClassesParams)
                                          : CompletableFuture[ScalaTestClassesResult] = {
    val future = new CompletableFuture[ScalaTestClassesResult]()
    val result = new ScalaTestClassesResult(List.empty.asJava)
    future.complete(result)

    future
  }

  override def buildTargetScalaMainClasses(scalaMainClassesParams: ScalaMainClassesParams)
                                          : CompletableFuture[ScalaMainClassesResult] = {
    val future = new CompletableFuture[ScalaMainClassesResult]()
    val result = new ScalaMainClassesResult(List.empty.asJava)
    future.complete(result)

    future
  }
}

object FuryBuildServer {
  class Cancelator { var cancel: () => Unit = () => () }

  case class Structure(modules: Map[ModuleRef, Module],
                       graph: Map[ModuleRef, List[ModuleRef]],
                       checkouts: Checkouts,
                       targets: Map[ModuleRef, Target]) {

    private[this] val hashes: mutable.HashMap[ModuleRef, Digest] = new mutable.HashMap()

    // TODO unify this with Compilation.hash
    def hash(ref: ModuleRef): Digest = {
      val target = targets(ref)
      hashes.getOrElseUpdate(
        ref, (target.kind, target.main, target.plugin, target.checkouts, target.binaries, target.dependencies,
            target.compiler.map { c => hash(c.ref) }, target.params, target.intransitive, target.sourcePaths,
            graph(ref).map(hash)).digest[Md5]
      )
    }

    def buildTarget(ref: ModuleRef): BuildTargetIdentifier = {
      val uri = s"fury:${hash(ref).toString}"
      new BuildTargetIdentifier(uri.toString)
    }

    def moduleRef(bti: BuildTargetIdentifier): Try[ModuleRef] = {
      val id = new URI(bti.getUri).getSchemeSpecificPart

      // TODO depends on assumption about how digest is encoded
      val bytes = java.util.Base64.getDecoder.decode(id)
      
      val digest = Digest(Bytes(bytes))
      
      modules.find { case (ref, _) => hash(ref) == digest }.map(_._1).ascribe(ItemNotFound(ModuleId(id)))
    }
  }

  private def toTarget(target: Target, struct: Structure) = {
    val ref = target.ref
    val id = struct.buildTarget(target.ref)
    val tags = List(moduleKindToBuildTargetTag(target.kind))
    val languageIds = List("java", "scala") // TODO get these from somewhere?
    val dependencies = target.dependencies.map(_.ref).map(struct.buildTarget)
    val capabilities = new BuildTargetCapabilities(true, false, false)

    val buildTarget = new BuildTarget(id, tags.asJava, languageIds.asJava, dependencies.asJava, capabilities)
    buildTarget.setDisplayName(moduleRefDisplayName(ref))

    for {
      compiler <- target.compiler
      bs       <- compiler.bloopSpec
               if compiler.binaries.nonEmpty
    } yield {
      val libs = compiler.binaries.map(_.javaPath.toAbsolutePath.toUri.toString).asJava
      val scalaBuildTarget = new ScalaBuildTarget(bs.org, bs.version, bs.version, ScalaPlatform.JVM, libs)
      buildTarget.setDataKind(BuildTargetDataKind.SCALA)

      buildTarget.setData(scalaBuildTarget)
    }

    buildTarget
  }

  private def moduleRefDisplayName(moduleRef: ModuleRef): String =
    s"${moduleRef.projectId.key}/${moduleRef.moduleId.key}"

  private def moduleKindToBuildTargetTag(kind: Kind): String =
    kind match {
      case Library           => BuildTargetTag.LIBRARY
      case Application       => BuildTargetTag.APPLICATION
      case Benchmarks        => BuildTargetTag.BENCHMARK
      case Compiler | Plugin => BuildTargetTag.LIBRARY // mark these NO_IDE?
    }

  private def toCompletableFuture[T](f: Future[T])(implicit ec: ExecutionContext): CompletableFuture[T] = {
    val result = new CompletableFuture[T]()
    f.foreach(result.complete)
    f.failed.foreach(result.completeExceptionally)
    result
  }

  private def get[T](t: Try[CompletableFuture[T]]): CompletableFuture[T] = {
    t.recover[CompletableFuture[T]]{ case e =>
      val future = new CompletableFuture[T]()
      future.completeExceptionally(e)
      future
    }.get
  }
  
  private class BspReporter(client: BuildClient) extends Reporter("bsp") {
    import MessageType._
    private def info(message: UserMsg)(implicit theme: Theme) =
      client.onBuildLogMessage(new LogMessageParams(INFORMATION, message.string(theme)))
    
    override def report(graph: Target.Graph, theme: Theme, multiplexer: Multiplexer[ModuleRef, CompileEvent])
                       (implicit log: Log): Unit = {

      implicit val t: Theme = theme
      multiplexer.stream(50, Some(Tick)).foreach {
        case StartCompile(ref)                           => info(msg"Starting compilation of module $ref")
        case StopCompile(ref, true)                      => info(msg"Successfully compiled module $ref")
        case StopCompile(ref, false)                     => info(msg"Compilation of module $ref failed")
        case DiagnosticMsg(ref, message)                 => info(message.msg)
        case Print(ref, line)                            => info(str"$line")
        case other                                       => ()
      }
    }
  }
}

case class BspTarget(id: BuildTargetIdentifier) extends Key(msg"BuildTargetIdentifier") {
  override def key: String = id.getUri
}

object BspTarget {
  implicit val msgShow: MsgShow[BspTarget] = v => UserMsg(_.url(v.key))
}
