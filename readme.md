<img src="doc/logo/render_github.png" alt="Fury">

![Build](https://github.com/propensive/fury/workflows/Build/badge.svg?branch=master)

## Overview

Fury is a long-term project to create next-generation build tooling to address the greatest challenges of
building software in an ever-changing environment, while maintaining the predictability, reliability and
simplicity of builds.

## Features
- fully data-oriented model for builds
- builds may be defined in terms of sources or binary dependencies
- power to control a build and the builds of all its source dependencies
- advanced support for conflict resolution amongst dependencies
- intuitive command-line API, with visual interfaces available later (Milestone 3)
- hierarchical model of builds and build distribution
- support for compiling Scala 2.x, Scala 3, Scala.js and Java
- composable variants of builds, for generalized cross-building (Milestone 2)
- support for running Docker containers as part of a build (Milestone 2)
- compilation distributed over the network (Milestone 3)
- optionally external builds, shared independently of source code

## Release

Fury is approaching its first public milestone release, _Alecto_. This will be the first release of Fury capable
of building a majority of Scala projects. Fury’s future plans are described in
[the roadmap](https://propensive.com/opensource/fury/roadmap).

## Installation

Pre-release versions of Fury may be installed by running,
```sh
curl -Ls fury.build | sh
```
in a shell.

For full details, see the [installation instructions](doc/install.md).

## Website
Full information about Fury is available on the [Fury website](https://propensive.com/opensource/fury).
