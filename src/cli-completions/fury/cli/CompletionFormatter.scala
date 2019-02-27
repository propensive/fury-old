package fury.cli
import fury.cli.Parameter.isFlag
import fury.cli.ShellCompletion.{MenuCompletion, ParameterNameCompletion, ParameterValueCompletion}

private[cli] trait CompletionFormatter {
  def format(c: ShellCompletion): Seq[String]
}

private[cli] object CompletionFormatter {

  def format(cs: Seq[ShellCompletion])(shell: Shell, offset: Int): Seq[String] = {
    val formatter = shell match {
      case Shell.Bash => BashFormat
      case Shell.Fish => FishFormat
      case Shell.Zsh  => new ZshFormat(offset)
    }

    cs.flatMap(formatter.format)
  }

  object BashFormat extends CompletionFormatter {

    def format(c: ShellCompletion): Seq[String] = c match {
      case MenuCompletion(_, commands)            => commands.map(_.name).sorted
      case ParameterNameCompletion(name, _, _)    => Seq(name)
      case ParameterValueCompletion(_, _, values) => values.map(_.value).sorted
    }
  }

  object FishFormat extends CompletionFormatter {
    override def format(c: ShellCompletion): Seq[String] = Nil
  }

  final class ZshFormat(offset: Int) extends CompletionFormatter {

    override def format(c: ShellCompletion): Seq[String] = c match {
      case MenuCompletion(menu, commands) =>
        val subMenus = commands.map(c => s"${c.name}:'${c.description}'")
        s"${offset - 1}:${menu.description}:((${subMenus.mkString(" ")}))" :: Nil

      case ParameterNameCompletion(name, p, values)  => formatParameter(name, p, values)
      case ParameterValueCompletion(name, p, values) => formatParameter(name, p, values)
    }

    private def formatParameter(
        name: String,
        parameter: Parameter[_],
        values: Seq[ParameterValue]
      ): Seq[String] = {
      val parameterPart = s"$name[${parameter.description}]"
      val argumentPart =
        if (isFlag(parameter)) ""
        else s":${parameter.argumentDescription}:((${values.map(print).mkString(" ")}))"

      Seq(parameterPart + argumentPart)
    }

    private def print(value: ParameterValue): String =
      value.description match {
        case None              => value.value
        case Some(description) => s"${value.value} \\:'$description'"
      }
  }
}
