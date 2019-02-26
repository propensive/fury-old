package fury.cli

import fury.cli.Parameter.{Declaration, InvalidParameter}

import scala.collection.immutable.SortedSet
import scala.util._

sealed trait Parameter[A] {
  def apply(arguments: Arguments): Try[A]

  final def description: String         = declaration.description
  final def name: String                = declaration.name
  final def aliases: Set[String]        = declaration.aliases
  final def argumentDescription: String = declaration.argumentDescription

  final def :+[B <: Parameter[_]](that: B): Parameters[this.type with that.type] =
    new Parameters(Set(this, that))

  protected def declaration: Declaration
}

private[cli] case class Flag(declaration: Declaration) extends Parameter[Boolean] {
  override def apply(arguments: Arguments): Try[Boolean] =
    arguments.valueOf(this) match {
      case Some(_) => Try(true)
      case None    => Try(false)
    }
}

// TODO - it is "named" (as in "--name value"), but what else can it be called?
private[cli] case class Named[A](
    declaration: Declaration
  )(extractor: String => Option[A],
    fallback: Option[Success[A]] = None)
    extends Parameter[A] {
  override def apply(arguments: Arguments): Try[A] =
    arguments.valueOf(this) match {
      case Some(value) => extractFrom(value)
      case None        => fallback.getOrElse(Failure(InvalidParameter(this)))
    }

  private def extractFrom(value: String): Try[A] = extractor(value) match {
    case Some(extracted) => Try(extracted)
    case None            => Failure(InvalidParameter(this, value))
  }
}

object Parameter {
  type Extractor[A] = String => Option[A]

  def isFlag(parameter: Parameter[_]): Boolean = parameter match {
    case _ @Flag(_) => true
    case _          => false
  }

  def flag(longName: String, shortName: String, description: String): Parameter[Boolean] = {
    val declaration = createDeclaration(longName, shortName, description)
    Flag(declaration)
  }

  private def createDeclaration(
      longName: String,
      shortName: String,
      description: String
    ): Declaration =
    Declaration(
        longName,
        SortedSet(s"--$longName", s"-$shortName"),
        description
    )

  def parameter[A](
      longName: String,
      shortName: String,
      description: String
    )(extractor: Extractor[A]
    ): Parameter[A] = {
    val declaration = createDeclaration(longName, shortName, description)
    Named(declaration)(extractor)
  }

  def optional[A](
      longName: String,
      shortName: String,
      description: String
    )(extractor: Extractor[A]
    ): Parameter[Option[A]] = {
    val declaration = createDeclaration(longName, shortName, description)
    Named(declaration)(extractor.andThen(Some.apply), Some(Success(None)))
  }

  case class InvalidParameter(parameter: Parameter[_], value: String = "") extends Exception

  /**
    * name: someName
    * aliases: (--someName, -s) TODO find better name
    */
  case class Declaration(name: String, aliases: Set[String], description: String) {

    def argumentDescription: String =
      name.dropWhile(_ == '-') // TODO move to a field
  }
}
