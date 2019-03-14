package fury

import java.nio.file.Files
import java.nio.file.Path

import fury.core._
import fury.strings._

import probably._

object TablesTest extends TestApp {
  override def tests(): Unit = {
    test("contextString with showSchema=true") {
      Tables(Config())
    }.assert { tables =>
      tables.contextString(msg"foo", true, msg"bar", msg"baz").string(Theme.NoColor) == "foo/bar/baz"
    }

    test("contextString with showSchema=false") {
      Tables(Config())
    }.assert { tables =>
      tables.contextString(msg"foo", false, msg"bar", msg"baz").string(Theme.NoColor) == "foo/baz"
    }
  }
}
