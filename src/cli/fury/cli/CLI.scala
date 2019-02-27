package fury.cli

import fury.cli.CLI._

sealed trait CLI[A] {
  def name: String
  def description: String

  final def execute(args: Seq[String]): CLI.Result[A] = {
    val executor = new Executor[A]
    executor.visit(this, args)
  }

  private[cli] def traverse[B](args: Seq[String], v: Visitor[A, B]): B
}

final class Command[A](val name: String, val description: String, val action: Action[_, A])
    extends CLI[A] {
  private[cli] def traverse[B](arguments: Seq[String], v: Visitor[A, B]): B =
    v.visit(this, arguments)
}

final class Menu[A](
      val name: String,
      val description: String,
      val commands: SubCommands[A],
      val default: String)
      extends CLI[A] {

    override private[cli] def traverse[B](args: Seq[String], v: Visitor[A, B]): B =
      args match {
        case Seq()       => visitSubCommand(default, args, v)
        case "--" +: _   => visitSubCommand(default, args, v)
        case cmd +: tail => visitSubCommand(cmd, tail, v)
      }

    private def visitSubCommand[B](commandName: String, args: Seq[String], v: Visitor[A, B]): B =
      commands.get(commandName) match {
        case Some(subCommand)                   => v.visit(subCommand, args)
        case None if isHelpCommand(commandName) => v.onUnclearCommand(this)
        case None                               => v.onUnknownArgument(this, commandName)
      }

    private def isHelpCommand(cmd: String): Boolean =
      CLI.helpCommands.contains(cmd)
  }

object CLI {
  private[cli] type SubCommand[T] = (String, CLI[T])
  private[cli] type SubCommands[T] = Map[String, CLI[T]]
  private[cli] val helpCommands: Set[String] = Set("help", "-h", "--help")

  trait Result[A]
  case class Success[A](value: A) extends Result[A]
  case class Failure[A](cause: Throwable)          extends Result[A]
  case class CommandUnclear[A](dispatcher: CLI[_]) extends RuntimeException with Result[A]

  def menu[T](name: String, default: String)(items: SubCommand[T]*): Menu[T] =
    new Menu(name, name, Map(items: _*), default)

  def menu[T](
      name: String,
      description: String,
      default: String
    )(items: SubCommand[T]*
    ): SubCommand[T] =
    name -> new Menu(name, description, Map(items: _*), default)

  def command[T](name: String, description: String, result: T): SubCommand[T] =
    command(name, description, Action(result))

  def command[T](name: String, description: String, action: Action[_, T]): SubCommand[T] =
    name -> new Command[T](name, description, action)

  private[cli] trait Visitor[A, B] {
    def onMenu(menu: Menu[A], arguments: Seq[String]): B
    def onCommand(command: Command[A], arguments: Seq[String]): B
    def onUnclearCommand(menu: Menu[A]): B
    def onUnknownArgument(menu: Menu[A], argument: String): B

    final def visit(cli: CLI[A], arguments: Seq[String]): B =
      cli match {
        case c: Command[A] => onCommand(c, arguments)
        case menu: Menu[A] => onMenu(menu, arguments)
      }
  }

  final class Executor[A] extends Visitor[A, Result[A]] {
    override def onCommand(command: Command[A], arguments: Seq[String]): Result[A] =
      command.action.execute(arguments) match {
        case scala.util.Success(value) => Success(value)
        case scala.util.Failure(cause) => Failure(cause)
      }

    override def onMenu(menu: Menu[A], args: Seq[String]): Result[A] =
      menu.traverse(args, this)
    override def onUnclearCommand(menu: Menu[A]): Result[A] =
      CLI.CommandUnclear(menu)
    override def onUnknownArgument(menu: Menu[A], argument: String): Result[A] =
      CLI.Failure(InvalidArgument(menu, argument))
  }

  case class InvalidArgument(node: Menu[_], argument: String) extends Exception
}
