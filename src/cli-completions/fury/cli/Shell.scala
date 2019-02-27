package fury.cli

sealed private[cli] trait Shell

private[cli] object Shell {

  def parse(name: String): Option[Shell] = name.toLowerCase match {
    case "bash" => Some(Bash)
    case "zsh"  => Some(Zsh)
    case "fish" => Some(Fish)
    case _      => None
  }

  case object Bash extends Shell
  case object Zsh  extends Shell
  case object Fish extends Shell
}
