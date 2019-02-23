package fury

import org.openjdk.jmh._
import generators.bytecode._

import scala.util._

object Jmh {

  def instrument(bytecode: Path, sources: Path, resources: Path): Try[Unit] =
    Try(JmhBytecodeGenerator.main(Array(bytecode.value, sources.value, resources.value, "asm")))
}
