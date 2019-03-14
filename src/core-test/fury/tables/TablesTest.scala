package fury

import java.nio.file.Files
import java.nio.file.Path

import fury.core._
import fury.strings._

import probably._

object TablesTest extends TestApp {
  override def tests(): Unit = {
    test("contextString with showSchema=true") {
      val tables = Tables(Config())

      tables.contextString(msg"foo", true, msg"bar", msg"baz").string(Theme.NoColor)
    }.assert {
      _ == "foo/bar/baz"
    }

    test("contextString with showSchema=false") {
      val tables = Tables(Config())

      tables.contextString(msg"foo", false, msg"bar", msg"baz").string(Theme.NoColor)
    }.assert {
      _ == "foo/baz"
    }
  }
}
