/*
  Fury, version 0.4.0. Copyright 2018-19 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
 */
package fury

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.util.Collections
import java.util.concurrent.{CompletableFuture, Future}

import ch.epfl.scala.bsp4j
import ch.epfl.scala.bsp4j._
import org.eclipse.lsp4j.jsonrpc.Launcher

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}


object Bsp {

  def createConfig(ctx: BspCli.Context): Try[ExitStatus] = {
    val bspDir = ctx.layout.bspDir.extant()
    val bspConfig = bspDir / "fury.json"
    val write = for {
      configFile <- ~Files.write(
        bspConfig.javaPath,
        bspConfigJson(ctx).serialize.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING)
    } yield {
      println(s"BSP config written to $configFile")
      Done
    }

    write.recover {
      case err: Throwable =>
        err.printStackTrace(System.err)
        Abort
    }
  }

  def bspConfigJson(ctx: BspCli.Context): Json = {
    // FIXME we should use fury launch jar directly with java here
    val furyExecutable = ctx.layout.home / "fury-0.4.0" / "bin" / "fury"

    Json(
      name = "Fury",
      argv = List(
        furyExecutable.javaPath.toAbsolutePath.toString,
        "direct", "bsp", "run"
      ),
      version = "0.4.0-dev",
      bspVersion = "2.0",
      languages = List("java","scala")
    )
  }

  def startServer(ctx: BspCli.Context): Try[ExitStatus] = {
    val waiting = for {
      running <- ~run(System.in, System.out, ctx.layout)
    } yield {
      System.err.println("started bsp process ...")
      running.get()
    }

    waiting
      .map { _ =>
        System.err.println("completed bsp process")
        Done
      }
      .recover {
        case err: Throwable =>
          System.err.println(s"bsp process encountered problem: ${err.getMessage}")
          err.printStackTrace(System.err)
          Abort
      }
  }


  def run(in: InputStream, out: OutputStream, layout: Layout): Future[Void] = {

    val server = new FuryBuildServer(layout)

    val launcher = new Launcher.Builder[BuildClient]()
      .setRemoteInterface(classOf[BuildClient])
      .setLocalService(server)
      .setInput(in)
      .setOutput(out)
      .create()

    server.onConnectWithClient(launcher.getRemoteProxy)

    launcher.startListening()
  }

}

object BspCli {
  case class Context(cli: Cli[CliParam[_]], layout: Layout)

  def context(cli: Cli[CliParam[_]]): Try[Context] =
    for {
      layout <- cli.layout
    } yield Context(cli, layout)
}

class FuryBuildServer(layout: Layout) extends BuildServer {
  import FuryBuildServer._


  override def buildInitialize(initializeBuildParams: InitializeBuildParams): CompletableFuture[InitializeBuildResult] = {

    System.err.println("**> buildInitialize")

    val capabilities = new BuildServerCapabilities()
    capabilities.setBuildTargetChangedProvider(false)
    val compileProvider = new CompileProvider(List("java","scala").asJava)
    capabilities.setCompileProvider(compileProvider)
    capabilities.setDependencySourcesProvider(false)
    capabilities.setInverseSourcesProvider(false)
    capabilities.setResourcesProvider(false)
    val runProvider = new RunProvider(List.empty[String].asJava) // TODO fury can provide run
    capabilities.setRunProvider(runProvider)
    val testProvider = new TestProvider(List.empty[String].asJava) // fury does not yet have a dedicated test interface
    capabilities.setTestProvider(testProvider)

    val result = new InitializeBuildResult("Fury", "0.4.0-dev", "2.0.0", capabilities)
    val future = new CompletableFuture[InitializeBuildResult]()
    future.complete(result)
    future
  }

  override def onBuildInitialized(): Unit = {
    // TODO
  }

  override def onBuildExit(): Unit = {
    // TODO
  }

  override def buildShutdown(): CompletableFuture[AnyRef] = {
    System.err.println("**> buildShutdown")
    // TODO
    val result = new CompletableFuture[AnyRef]()
    result.complete("shutdown elided") // FIXME ensure shutdown
    result
  }

  override def workspaceBuildTargets(): CompletableFuture[WorkspaceBuildTargetsResult] = {

    System.err.println("**> workspaceBuildTargets")

    val config = Config()
    val io = new Io(System.out, config)

    val result = for {
      layer <- Layer.read(io, layout.furyConfig, layout)
      projects <- layer.projects
    } yield {
      // TODO respect current schema
      val targets = projects
        .flatMap(_.modules)
        .map(moduletoTarget)
        .toList

      new WorkspaceBuildTargetsResult(targets.asJava)
    }

    val future = new CompletableFuture[WorkspaceBuildTargetsResult]()

    result match {
      case Success(value) => future.complete(value)
      case Failure(exception) => future.completeExceptionally(exception)
    }

    future
  }

  override def buildTargetSources(sourcesParams: SourcesParams): CompletableFuture[SourcesResult] = {

    System.err.println("**> buildTargetSources")

    // TODO get sources for modules corresponding to targets
    val res = new SourcesResult(Collections.emptyList())
    val future = new CompletableFuture[SourcesResult]()
    future.complete(res)
    future
  }

  override def buildTargetInverseSources(inverseSourcesParams: InverseSourcesParams): CompletableFuture[InverseSourcesResult] = {
    // TODO
    val result = new CompletableFuture[InverseSourcesResult]()
    result.completeExceptionally(new NotImplementedError("method not implemented"))
    result
  }

  override def buildTargetDependencySources(dependencySourcesParams: DependencySourcesParams): CompletableFuture[DependencySourcesResult] = {
    // TODO
    val result = new CompletableFuture[DependencySourcesResult]()
    result.completeExceptionally(new NotImplementedError("method not implemented"))
    result
  }

  override def buildTargetResources(resourcesParams: ResourcesParams): CompletableFuture[ResourcesResult] = {
    // TODO
    val result = new CompletableFuture[ResourcesResult]()
    result.completeExceptionally(new NotImplementedError("method not implemented"))
    result
  }

  override def buildTargetCompile(compileParams: CompileParams): CompletableFuture[bsp4j.CompileResult] = {
    // TODO
    val result = new CompletableFuture[bsp4j.CompileResult]()
    result.completeExceptionally(new NotImplementedError("method not implemented"))
    result
  }

  override def buildTargetTest(testParams: TestParams): CompletableFuture[TestResult] = {
    val result = new CompletableFuture[TestResult]()
    result.completeExceptionally(new NotImplementedError("method not implemented"))
    result
  }

  override def buildTargetRun(runParams: RunParams): CompletableFuture[RunResult] = {
    val result = new CompletableFuture[RunResult]()
    result.completeExceptionally(new NotImplementedError("method not implemented"))
    result
  }

  override def buildTargetCleanCache(cleanCacheParams: CleanCacheParams): CompletableFuture[CleanCacheResult] = {
    // TODO
    val result = new CompletableFuture[CleanCacheResult]()
    result.completeExceptionally(new NotImplementedError("method not implemented"))
    result
  }
}

object FuryBuildServer {

  private def moduletoTarget(module: Module) = {
    val id = moduleIdToBuildTargetIdentifier(module.id)
    val tags = List(moduleKindToBuildTargetTag(module.kind))
    val languageIds = List("java", "scala") // TODO get these from somewhere?
    val dependencies = module.compilerDependencies.map(d => moduleIdToBuildTargetIdentifier(d.moduleId))
    val capabilities = new BuildTargetCapabilities(true, false, false)

    val target = new BuildTarget(id, tags.asJava, languageIds.asJava, dependencies.toList.asJava, capabilities)
    target
  }

  private def moduleIdToBuildTargetIdentifier(moduleRef: ModuleId) =
    new BuildTargetIdentifier(moduleRef.key)

  private def moduleKindToBuildTargetTag(kind: Kind): String =
    kind match {
      case Library => BuildTargetTag.LIBRARY
      case Application => BuildTargetTag.APPLICATION
      case Benchmarks => BuildTargetTag.BENCHMARK
      case Compiler | Plugin => BuildTargetTag.LIBRARY // mark these NO_IDE?
    }
}
