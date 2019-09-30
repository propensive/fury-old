/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Fury, version 0.6.7. Copyright 2018-19 Jon Pretty, Propensive OÜ.                                         ║
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
package fury

import java.util.concurrent.atomic.AtomicBoolean

import _root_.io.methvin.better.files.RecursiveFileMonitor
import better.files.File
import fury.core._
import fury.io.Path
import fury.model._
import fury.strings._
import fury.utils.Multiplexer

import scala.concurrent.{Await, ExecutionContext, Future, duration}
import scala.concurrent.ExecutionContext.Implicits.global

object RebuildService {
  def repeatBuild(io: Io, compilation: Compilation, project: Project, module: Module, layout: Layout, 
                  policy: Policy, reporter: Reporter, theme: Theme, compileArgs: List[String]): Unit = {
    val multiplexer = new Multiplexer[ModuleRef, CompileEvent](compilation.targets.map(_._1).to[List])
    val allDirectories = compilation.targets.values.flatMap(_.sourcePaths).toSet
    io.println(str"Watching ${allDirectories.size} directories...")
    val sourceWatcher = new SourceWatcher(io, allDirectories)
    val future = repeat(
      io,
      skip = () => !sourceWatcher.hasChanges,
      action = () => buildOnce(io, compilation, project, module, layout, policy, compileArgs, sourceWatcher, multiplexer)
    )
    reporter.report(io, compilation, theme, multiplexer)
    val compileResults = Await.result(future, duration.Duration.Inf)
    multiplexer.closeAll()
    sourceWatcher.stop()
  }
  
  private def buildOnce(io: Io, compilation: Compilation, project: Project, module: Module, 
                         layout: Layout, policy: Policy, compileArgs: List[String],
                        sourceWatcher: SourceWatcher, multiplexer: Multiplexer[ModuleRef, CompileEvent]) = {
    sourceWatcher.clear()
    val moduleRef = module.ref(project)
    io.println(msg"Recompiling $moduleRef")
    val compilations = compilation.compile(io, moduleRef, multiplexer, Map(), layout, policy, compileArgs)
    Future.sequence(compilations.values)
  }

  private def repeat[Res](io: Io, skip: () => Boolean, action: () => Future[Res]): Future[Res] = {
    for{
      x <- if (skip()) {
        io.print(Ansi.clear())
        io.print(Ansi.up(1)())
        io.println(str"Waiting for changes...")
        Thread.sleep(1000)
        repeat(io, skip, action)
      } else {
        action()
      }
      _ = io.println("Rebuild complete")
      y <- if(Thread.currentThread.isInterrupted) Future.successful(x) else repeat(io, skip, action)
    } yield y
  }
}

private class SourceWatcher(io: Io, sources: Set[Path]) {
  private[this] val changed = new AtomicBoolean(true)

  private[this] val watchers = sources.map( src => new RecursiveFileMonitor(File(src.value)) {
    override def onCreate(file: File, count: Int) = onChange(file)
    override def onModify(file: File, count: Int) = onChange(file)
    override def onDelete(file: File, count: Int) = onChange(file)

    override def start()(implicit executionContext: ExecutionContext): Unit = watcher.watchAsync(SourceWatcher.executor)
  })

  private[this] def onChange(file: File) = {
    val important: Boolean = file.extension.contains(".scala") || file.extension.contains(".java")
    if(important){
      io.println(str"File modified: ${Path(file.path)}")
      changed.set(true)
    }
  }

  watchers.foreach(_.start()(SourceWatcher.ec))

  def hasChanges: Boolean = changed.get

  def clear(): Unit = changed.set(false)

  def stop(): Unit = watchers.foreach(_.stop)

}

object SourceWatcher{
  val executor = java.util.concurrent.Executors.newCachedThreadPool()
  val ec: ExecutionContext = ExecutionContext.fromExecutor(executor, throw _)
}

