package fury

import org.openjdk.jmh._
import generators.bytecode._

import scala.util._
import java.io._

object Jmh {

  def instrument(bytecode: Path, sources: Path, resources: Path): Try[Unit] = {
    val discard: OutputStream = _ => ()
    val out                   = System.out
    System.setOut(new PrintStream(discard))
    val result = Try(
        JmhBytecodeGenerator.main(Array(bytecode.value, sources.value, resources.value, "asm")))
    System.setOut(out)
    result
  }
}
