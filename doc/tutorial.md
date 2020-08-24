# Fury Tutorial

The easiest way to start a new Scala project with Fury is to clone an existing template. You can call your
project anything you like, but for this tutorial we will call it `sample`. Start by running,
```
fury layer clone -l propensive/scala-new -f sample
cd sample
```

This will set up a new Fury layer, already set up for a Scala project, in the directory `sample`. We should
start by renaming the default project:
```
fury project update -n `sample`
```

This quickstart project is set up to look for Scala source files in the `src/core` directory, so let's create it
with,
```sh
mkdir -p src/core
```
and create the file, `src/core/hello.scala` with the following content:

```scala
object Main {
  def main(args: Array[String]): Unit =
    println("Hello World!")
}
```

We can now compile this by running,
```
fury
```

This will not, however, _run_ the application. To do that, we must first change it from a `lib` module to an
`app` module, specifying its main method, like so,
```sh
fury module update -t app -M Main
```

Running `fury` again will now additionally run the code.