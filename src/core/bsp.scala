/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.0. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package fury.core

import fury.strings._, fury.jsongen._, fury.io.Path

import java.io.{InputStream, OutputStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.util.concurrent.{CompletableFuture, Future}

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j._
import FuryBuildServer._
import gastronomy.{Bytes, Digest, Md5}
import guillotine._
import mercator._
import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

import language.higherKinds

object Bsp {

  val bspVersion = "2.0.0-M4"

  def createConfig(layout: Layout): Try[Unit] = {
    val bspDir    = layout.bspDir.extant()
    val bspConfig = bspDir / "fury.json"

    for {
      // FIXME we should use fury launch jar directly with java here
      fury   <- whichFury(layout)
      config = bspConfigJson(fury)
      _      <- bspConfig.writeSync(config.serialize)
    } yield ()
  }

  private def whichFury(layout: Layout): Try[Path] = {
    implicit val env = layout.env
    sh"command -v fury".exec[Try[String]].map(Path.apply)
  }

  private def bspConfigJson(fury: Path): Json =
    Json(
        name = "Fury",
        argv = List(
            fury.javaPath.toAbsolutePath.toString,
            "standalone",
            "bsp"
        ),
        version = Version.current,
        bspVersion = bspVersion,
        languages = List("java", "scala")
    )

  def startServer(cli: Cli[CliParam[_]]): Try[ExitStatus] =
    for {
      layout  <- cli.layout
      running <- ~run(System.in, System.out, layout)
    } yield {
      System.err.println("Started bsp process ...")
      running.get()
      Done
    }

  def run(in: InputStream, out: OutputStream, layout: Layout): Future[Void] = {

    val cancel = new Cancelator()
    val server = new FuryBuildServer(layout, cancel)

    val launcher = new Launcher.Builder[BuildClient]()
      .setRemoteInterface(classOf[BuildClient])
      .setLocalService(server)
      .setInput(in)
      .setOutput(out)
      .create()

    server.onConnectWithClient(launcher.getRemoteProxy)

    val listening = launcher.startListening()
    cancel.cancel = () => listening.cancel(true) // YOLO
    listening
  }

}

class FuryBuildServer(layout: Layout, cancel: Cancelator) extends BuildServer with ScalaBuildServer {
  import FuryBuildServer._

  private val config = Config()
  private val io     = new Io(System.err, config)

  private def structure: Try[Structure] =
    for {
      layer          <- Layer.read(io, layout.furyConfig, layout)
      schema         <- layer.mainSchema
      hierarchy      <- schema.hierarchy(io, layout.pwd, layout)
      universe       <- hierarchy.universe
      projects       <- layer.projects
      graph          <- projects.flatMap(_.moduleRefs).map { ref =>
                          for {
                            ds   <- universe.dependencies(io, ref, layout)
                            arts <- (ds + ref).map { d => universe.makeTarget(io, d, layout) }.sequence
                          } yield arts.map { a =>
                            (a.ref, (a.dependencies.map(_.ref): List[ModuleRef]) ++ (a.compiler
                                .map(_.ref.hide): Option[ModuleRef]))
                          }
                        }.sequence.map(_.flatten.toMap)
      allModuleRefs  = graph.keys
      modules       <- allModuleRefs.traverse { ref => universe.getMod(ref).map((ref, _)) }
      targets       <- graph.keys.map { key =>
                         universe.makeTarget(io, key, layout).map(key -> _)
                       }.sequence.map(_.toMap)
      checkouts     <- graph.keys.map(universe.checkout(_, layout)).sequence
    } yield Structure(modules.toMap, graph, checkouts.foldLeft(Set[Checkout]())(_ ++ _), targets)

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

    val result = new InitializeBuildResult("Fury", Version.current, Bsp.bspVersion, capabilities)
    val future = new CompletableFuture[InitializeBuildResult]()
    future.complete(result)
    
    future
  }

  override def onBuildInitialized(): Unit = {
    // TODO
  }

  override def onBuildExit(): Unit = {
    io.println("**> buildExit")
    cancel.cancel() // YOLO?
  }

  override def buildShutdown(): CompletableFuture[AnyRef] = {
    io.println("**> buildShutdown")
    val result = new CompletableFuture[AnyRef]()
    result.complete("just exit already")
    result
  }

  override def workspaceBuildTargets(): CompletableFuture[WorkspaceBuildTargetsResult] = {

    io.println("**> workspaceBuildTargets")

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

    io.println("**> buildTargetSources")

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

    io.println("**> buildTargetDependencySources")

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
    future.completeExceptionally(new NotImplementedError("method not implemented"))
    
    future
  }

  override def buildTargetCompile(compileParams: CompileParams): CompletableFuture[bsp4j.CompileResult] = {
    // TODO MVP
    val future = new CompletableFuture[bsp4j.CompileResult]()
    val result = new bsp4j.CompileResult(StatusCode.CANCELLED)
    future.complete(result)

    future
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
    // TODO fury supports cleaning
    val future = new CompletableFuture[CleanCacheResult]()
    future.completeExceptionally(new NotImplementedError("method not implemented"))
    
    future
  }

  private def scalacOptionsItem(target: BuildTargetIdentifier, struct: Structure): Try[ScalacOptionsItem] =
    struct.moduleRef(target).map { ref =>
      val art = struct.targets(ref)
      val params = art.params.map(_.parameter)
      val paths = art.binaries.map(_.javaPath.toUri.toString)
      val classesDir = layout.classesDir.javaPath.toAbsolutePath.toUri.toString
      
      new ScalacOptionsItem(target, params.asJava, paths.asJava, classesDir)
    }

  override def buildTargetScalacOptions(scalacOptionsParams: ScalacOptionsParams)
                                       : CompletableFuture[ScalacOptionsResult] = {

    io.println("**> buildTargetScalacOptions")

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
                       checkouts: Set[Checkout],
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
      val bytes  = java.util.Base64.getDecoder.decode(id)
      
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
}

case class BspTarget(id: BuildTargetIdentifier) extends Key(msg"BuildTargetIdentifier") {
  override def key: String = id.getUri
}

object BspTarget {
  implicit val msgShow: MsgShow[BspTarget] = v => UserMsg(_.url(v.key))
}
