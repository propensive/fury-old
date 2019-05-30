package fury.installer

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.Files

import guillotine._
import fury.io._
import environments.none

import scala.io.StdIn
import scala.util.Try

object Installer {

  private val furyVersion = {
    (new BufferedReader(new InputStreamReader(getClass.getClassLoader.getResourceAsStream("fury/.version")))).readLine
  }
  private val home = Path(System.getProperty("user.home"))
  private val config = home / ".furyrc"
  private var destination: Path = home / s"fury-$furyVersion"

  def main(args: Array[String]): Unit = {
    prepare
    untarPayload
    updateShells
    restartFury
    completion
  }

  private def prepare: Unit = {
    val dest = StdIn.readLine(s"Where should Fury be installed? [${destination.value}] ")
    if(dest != null && dest.trim != "") destination = Path(dest)
    if(destination.javaPath.toFile.isDirectory){
      val overwrite = StdIn.readLine(s"Target directory ${destination} already exists. Overwrite? (Y/n)")
      if(overwrite == null || overwrite == "" || overwrite.toLowerCase == "y"){
        destination.delete.get
      }
    }
    destination.extant()
  }

  private def untarPayload: Unit = {
    val bundleName = s"fury-$furyVersion.tar.gz"
    val bundle = getClass.getClassLoader.getResourceAsStream(bundleName)
    Files.copy(bundle, (destination / "out.tar.gz").javaPath)
    val extract = sh"tar xCzf ${destination.value} ${(destination / "out.tar.gz").value}"
    val result = extract.exec[Try[String]].get
  }

  private def updateShells: Unit = {
    println("Updating shell configuration")
    config.extant()
    (destination / "etc" / "aliases").copyTo(config / "aliases")
    updateBash
    updateZsh
    updateFish
  }

  private def updateBash: Unit = {
    val rcFile = home / ".bashrc"
    val rcBackup = home / ".bashrc.bak"
    val commands = config / "bash"
    if(rcFile.exists){
      rcFile.copyTo(rcBackup)
      rcFile.writeSync("\n").get
      rcFile.writeSync("# Added by Fury\n").get
      rcFile.writeSync(s"export FURYHOME=${destination.absolutePath.get.value} && source ${commands.value} # Added by Fury").get
    }
    (destination / "etc" / "bashrc").copyTo(commands)
  }

  private def updateZsh: Unit = {
    val rcFile = home / ".zshrc"
    val rcBackup = home / ".zshrc.bak"
    val commands = config / "zsh"
    if(rcFile.exists){
      rcFile.copyTo(rcBackup)
      rcFile.writeSync("\n").get
      rcFile.writeSync("# Added by Fury\n").get
      rcFile.writeSync(s"export FURYHOME=${destination.absolutePath.get.value} && source ${commands.value} # Added by Fury").get
    }
    (destination / "etc" / "zshrc").copyTo(commands)
  }

  private def updateFish: Unit = {
    val rcFile = home / ".config" / "fish" / "config.fish"
    val rcBackup = home / ".config" / "fish" / "config.fish.bak"
    val commands = config / "fish"
    if(rcFile.exists){
      rcFile.copyTo(rcBackup)
      rcFile.writeSync("\n").get
      rcFile.writeSync("# Added by Fury\n").get
      rcFile.writeSync(s"set -Ux FURYHOME ${destination.absolutePath.get.value}; and source ${commands.value} # Added by Fury").get
    }
    (destination / "etc" / "fishrc").copyTo(commands)
  }

  private def restartFury: Unit = {
    val newFury = destination / "bin" / "fury"
    val check = sh"type -p fury"
    val stop = sh"fury stop"
    val start = sh"${newFury.value} start"
    check.exec[Try[String]].flatMap{
      _ => stop.exec[Try[String]]
    }
    start.exec[Try[String]].get
  }

  private def completion: Unit = {
    val banner =
      """
        |
        | ┌───────────────────────────────────────────────────────────────────┐
        | │                                                                   │
        | │  Fury is now installed. To test it, open a new terminal and run,  │
        | │                                                                   │
        | │  > fury about                                                     │
        | │                                                                   │
        | └───────────────────────────────────────────────────────────────────┘
        |
      """.stripMargin
    println(banner)
  }

}
