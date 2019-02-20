package fury

object Reporter {
  val all: List[Reporter] = List(GraphReporter, LinearReporter, QuietReporter)
  final private val reporters: Map[String, Reporter] = all.map { r =>
    r.name -> r
  }.toMap
  def unapply(string: String): Option[Reporter] = reporters.get(string)
  implicit val stringShow: StringShow[Reporter] = _.name
}

abstract class Reporter(val name: String) {

  def report(
      io: Io,
      compilation: Compilation,
      theme: Theme,
      multiplexer: Multiplexer[ModuleRef, CompileEvent],
      startTime: Long
    ): Unit
}

object GraphReporter extends Reporter("graph") {

  private def timeString(t: Long): String = if (t >= 10000) s"${t / 1000}s" else s"${t}ms"

  def report(
      io: Io,
      compilation: Compilation,
      theme: Theme,
      multiplexer: Multiplexer[ModuleRef, CompileEvent],
      startTime: Long
    ): Unit = {
    Graph.live(
        changed = false,
        io,
        compilation.allDependenciesGraph.mapValues(_.to[Set]),
        multiplexer.stream(50, Some(Tick)),
        Map())(theme)
    val duration = System.currentTimeMillis - startTime
    io.println(msg"Total time: ${timeString(duration)}")
  }
}

object LinearReporter extends Reporter("linear") {

  def report(
      io: Io,
      compilation: Compilation,
      theme: Theme,
      multiplexer: Multiplexer[ModuleRef, CompileEvent],
      startTime: Long
    ): Unit =
    multiplexer.stream(50, None).foreach {
      case StartCompile(ref)           => io.println(msg"Starting compilation of module $ref")
      case StopCompile(ref, out, true) => io.println(msg"Successfully compiled module $ref")
      case StopCompile(ref, out, false) =>
        io.println(msg"Compilation of module $ref failed. Compiler output:")
        io.println(out)
      case _ => ()
    }
}

object QuietReporter extends Reporter("quiet") {

  def report(
      io: Io,
      compilation: Compilation,
      theme: Theme,
      multiplexer: Multiplexer[ModuleRef, CompileEvent],
      startTime: Long
    ): Unit =
    multiplexer.stream(50, None).foreach { event =>
      ()
    }
}
