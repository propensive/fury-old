package fury.cli

import fury._

final class HelpPrinter(padded: String => String) {

  def print(c: CLI[_]): Seq[String] = c match {
    case cli.Menu(name, description, items, _) =>
      "" +: printKey(name, description) +: items.values.toSeq
        .sortBy(ordering)
        .flatMap(print)
    case command => printKey(command.name, command.description) :: Nil
  }

  private def ordering(menu: CLI[_]): (String, Int) =
    menu match {
      case _: Command[_] => (menu.name, 0)
      case _             => (menu.name, 1)
    }

  private def printKey(key: String, description: String): String =
    str"  ${padded(key)} $description"
}

object HelpPrinter {

  def print(dispatcher: CLI[_]): Seq[String] = HelpPrinter().print(dispatcher)

  def apply(width: Int = 12): HelpPrinter = {
    def padded: String => String = str => str.padTo(width, ' ')
    new HelpPrinter(padded)
  }
}
