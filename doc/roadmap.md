# Fury Roadmap

The development of Fury has come a long way since starting in 2018, and still has a long journey ahead of it,
but we are now closer to the release of version 1.0 than ever before. We plan _three_ milestone releases,
starting with _Alecto_ in September 2020.

## Milestone 1: Alecto (September 2020)

The first milestone build (Alecto) will be the first release of Fury with everything needed to build most Scala
libraries and applications.

### Vent your Fury!

You can now publish builds to the Fury's online build directory service, [vent.dev](https://vent.dev/). This
allows users with [GitHub](https://github.com) accounts to publish builds on _vent.dev_ in the style
`<group>/<project>`, where `<group>` is that user's account name, or any organization they are a member of.

Support for indicating breaking and non-breaking changes and expiration dates on published layers sets users'
expectations for upgrading, and for the long-term maintenance of a build.

### Hierarchical builds

Builds are organized into layers, which may import projects from other layers. Imported layers may now be
navigated and edited as easily as the main layer, making it possible to modify the definition of any project
used in any build.

### Scala.js

Fury now supports Scala.js, and allows the export of `.js` files.

### Universal updates

"Universal" updates enable the possibility to make changes to projects, layers and repositories _universally_
with a single command. This includes unifying or renaming projects, updating layer imports and updating Git
repository checkouts on the current layer, and all references elsewhere in the layer hierarchy, all at the same
time. This makes it very easy to maintain changes across an entire ecosystem.

### Workspaces

Alecto introduces a new concept of _workspaces_ into which different types of build artifact, such as JAR files,
may be _included_ for use in an application module. The module may subsequently produce its own artifacts, which
can be used as resources or sources.

### BSP support

Thanks to contributions from Justin Kaeser at JetBrains, Fury now runs as a BSP server.

## Milestone 2: Magaera (Q4, 2020)

Milestone 2 will introduce a variety of new features to expand Fury's capabilities.

### Improved Command-Line Interface

Support for Bash and Fish as well as Zsh will be provided, as well as more informative tab-completions wherever
possible, in particular, additional descriptive information may be provided on parameter values, which is
currently lacking.

### Binary publishing

The facility to store publishing details in builds, generate publish artifacts, and publish them to Maven
Central will be added.

### Docker support

A new module type, `container` will run a specified [Docker](https://docker.com/) image, with a mounted
workspace, allowing arbitrary processes to operate on build artifacts within a Docker container, and to produce
new build artifacts.

### Binary shading

To facilitate the coexistence of different versions of a dependency on the same classpath, Fury will provide the
option to _shade_ selected packages, rewriting bytecode as necessary.

### Improved Task Management

Fury's internal task management will be improved, including cancellation of builds, termination of
application modules, task isolation (e.g. for running benchmarks) and more reliable shutdowns.

### Layer Variations

General support for creating, publishing and depending upon variations of layers, supporting tweaks such as
compiler version changes, additional dependencies, tweaks to source directories or compiler options, enabling
more generalized support for cross-building.

### Distributed caching of builds

Using IPFS's publish-subscribe support, a local running instance of Fury will be able to fetch pre-built
artifacts from machines within a connected workgroup that has already built those artifacts.

## Milestone 3: Tisiphone (Q1, 2021)

Milestone 3 will focus on improved integration with external tools, and better distribution of the build
workload within connected workgroups.

### VS Code extension

Viewing, editing and launching Fury builds within Visual Studio Code will provide an alternative to Fury's
standard command-line interface. This will be provided through Fury's REST API.

### FUSE build navigation

Navigation of layers, projects and modules through a filesystem-like interface will provide another alternative
way to interact with builds, and may make shell-scripting easier.

### Distributed builds

Building upon the publish/subscribe support in _Magaera_, support for fully-distributed builds within a
connected workgroup will be provided, proactively initiating remote builds on connected machines.

## Version 1.0 (Q2/3, 2021)

Milestones 1, 2 and 3 will each introduce new features to Fury which progressively enhance its feature-set.
However, no new features will be introduced between Milestone 3 and version 1.0. Instead, version 1.0 will
focus on reliability, robustness, performance, and 

## Version 1.1 (Q4, 2021)

Version 1.1 will primarily migrate Fury's code from Scala 2 to Scala 3, as well as providing bugfixes to
version 1.0.