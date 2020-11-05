[<img src="https://img.shields.io/gitter/room/propensive/fury?color=f00762&style=for-the-badge" height="24">](https://gitter.im/propensive/fury)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/CHCPjERybv)
[<img src="https://img.shields.io/matrix/propensive.fury:matrix.org?label=MATRIX&color=0dbd8b&style=for-the-badge" height="24">](https://app.element.io/#/room/#propensive.fury:matrix.org)
[<img src="https://img.shields.io/twitter/follow/propensive?color=%2300acee&label=TWITTER&style=for-the-badge" height="24">](https://twitter.com/propensive)
[<img src="https://img.shields.io/badge/Vent-propensive%2Ffury-f05662?style=for-the-badge" height="24">](https://vent.dev)

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

## Fury Concepts

Fury introduces a number of new concepts for developers. While full documentation is in the process of being
written, a [work-in-progress document](doc/concepts.md) provides a lot of detail on these new ideas and how they
work.

## Release

Fury is approaching its first public milestone release, _Alecto_. This will be the first release of Fury capable
of building a majority of Scala projects. Fury’s future plans are described in
[the roadmap](doc/roadmap.md).

## Installation

Pre-release versions of Fury (with no guarantees on features or reliability) may be installed by running,
```sh
curl -Ls fury.build | sh
```
in a shell.

For more details, see the full [installation instructions](doc/install.md).

## Getting Started

To set up a simple Fury project, follow the short [tutorial for Scala](doc/tutorial.md) or, if you prefer, the
[tutorial for Java](doc/tutorial-java.md).

## Website

Full information about Fury is available on the [Fury website](https://propensive.com/opensource/fury).

## Contributing

Contributors to Fury are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/fury/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Fury easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
the answers.

## Author

Fury was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).

## License

Fury is copyright &copy; 2018-20 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).

