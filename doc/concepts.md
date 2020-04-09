# Concepts

## Basic concepts

Fury builds are modeled as _layers_ which are the atoms of distribution. They may define _projects_ and inherit
them from imported layers. Projects consist of _modules_, which may depend on other modules forming a directed
acyclic graph which can span projects and layers. Modules compile _sources_ which are taken from a _repo_
(repository) using a compiler (which is just another module).

## Motivation and approach

Fury attempts to make the challenge of coordinating the maintenance of different parts of a constantly-evolving
ecosystem more manageable. This is inherently difficult: an ecosystem will have many different stakeholders,
with different availability and areas of interest, and while each maintainer will be aware of other projects
they depend on, they will often be blind to the projects which exist downstream of theirs, and the impact of
changes they may choose to make.

This is at least partially a social problem: communication and coordination between maintainers is a necessary
part of the maintenance process. But there is a tension amongst project maintainers between keeping a
consistent API unchanging to minimize the burden of maintenance on downstream projects, and introducing new
features and enhancements which may break compatibility. This is a tension between _stability_ and _progress_.

Fury does not pretend to have a clever trick to magically resolve this tension, but it seeks to mitigate the
_incidental_ problems so that users can focus on solving the _inherent_ problems. In particular, Fury:

- reduces the friction of publishing and forking libraries
- crowdsources the development and maintenance of builds
- distinguishes between breaking and nonbreaking changes
- distributes the burden of maintaining coherency across the ecosystem

### Easier publishing and forking

### 

### Semantic versioning of builds

###

## Anatomy of a Fury build

The topmost entity in the Fury build model is a layer. A layer contains all of the information necessary to
build something, and is somewhat similar to the concept of workspace in 
[Eclipse](https://www.eclipse.org/eclipseide).

A layer may reference other layers by _importing_ them. All projects defined in the imported layer 
(and also in any layers it references transitively) become available in your layer to inspect, use and even
edit.

A layer usually contains one or more _project_ definitions. A project is a set of sources and dependencies,
which usually represents an entire application or library with a single name, license. It is similar to the
concept of project in Eclipse, [IntelliJ](https://www.jetbrains.com/idea) or [Maven](http://maven.apache.org),
and will usually correspond to a single [Git](https://git-scm.com) repository, though this isn't enforced.

A project consists of one or more modules. A module corresponds to a set of tightly coupled sources that are
built with a single compiler task. This is the smallest unit of source organization in Fury, and is similar to
the concept of module in IntelliJ, Maven or [SBT](https://www.scala-sbt.org/1.x/docs).

Each module may (but doesn't have to) reference a set of source directories that are passed to the compiler
when the module is built. These sources may be located in remote Git repositories, or on the local file system,
though a layer which refers to local files may not be shared with other users.

A module may also have binary dependencies, which must be located in remote repositories (e. g.
[Maven Central](https://mvnrepository.com/repos/central) or [Bintray](https://jfrog.com/bintray)). When the
module is being built, its binary dependencies are downloaded and passed to the compiler.
[Coursier](https://get-coursier.io) is used to fetch transitive dependencies and resolve potential version
conflicts.

A module may also have dependencies on other modules defined in the layer (or its imported layers). These
modules are built prior to the current module, and their outputs (such as `*.class` files) passed to the
compiler. This is similar to how module dependencies work in Maven or SBT, but the scope of such links is not
limited to the project; each module may depend on any module accessible in the current layer and its imported
layers.

Among other things, a layer may contain references to remote source repositories. The layer does not use them
directly, but every module in the layer can reference them as source locations. As the module is being built,
the repositories it depends on are checked out to a specific commit. Repositories may also be configured to
"track" a branch, but this is merely a convenience to make it easier to manually update a repository to a newer
commit; for repeatability of builds, source references are always precisely defined by commit hashes.

#### Navigating Layers

Layers form a hierarchy: each may import projects from other layers, which themselves import projects from yet
further layers, and so on. And imports are _named_, usually with the same name as the layer's published name. So
every imported layer can be thought of as accessible at a filesystem-like path, starting from the root (`/`). A
more deeply-nested layer may be accessible at, for example,
```
/ecosystem/typelevel/shapeless/scala
```

Fury, necessarily, has this full hierarchy of layer definitions available to it. So it becomes very easy to view
and even edit other layers.

When you start a project, you will begin with a "root" layer, '/'. This one doesn't have a name (at least until
it is published, and someone imports it!), but every layer it imports, directly or transitivily, will. The
commands you run, such as listing projects, updating layers or running builds, will operate on this layer,
reading it, and potentially modifying it.

However it is possible to perform all these actions, just as easily, on any layer in the hierarchy, just by
navigating to that layer first.

A new layer can be selected with,
```
fury layer select --layer <layer path>
```

Assuming the layer exists, this will switch your context to the new layer, and all subsequent commands will
operate on the imported layer. You can change dependencies, run builds, or ever publish, as if you were working
on the imported layer directly.

From this new layer context, Fury will be blind to any _upstream_ layers, i.e. those which are importing your
layer. However, when you navigate back to the root layer, with,
```
fury layer select --layer /
```
any changes you had made in the imported layer will be reflected in the root layer.

This makes it very easy to make changes not just to your own build, but to any builds you depend upon.

### Projects

A project is a group of one or more modules which, collectively, are the component parts of a larger entity,
which (by any reasonable description) would be called a "project". The name of a project is what is typically
used to uniquely identify it in conversations, documentation and marketing. Fury uses the name of the project to
work out whether two dependencies from different layers are intended to be the same project or not.

While this grouping may not seem important, it is fundamental to Fury finding coherent build definitions for
every project the build depends on. If two projects coming from different layers have the same name, but
different definitions, then Fury considers them to be conflicting variants of the same project, which must be
resolved before the build may be run (or published).

Fury establishes a one-to-one mapping between project names and project definitions, which it calls a
_universe_.

#### Conflicts and uniqueness

#### Licenses

### Modules

Modules define units of compilation, and dependencies between them. Each represents a collection of sources
which should be compiled to produce a collection of outputs, but they are _unitary_ in the sense that
compilation will either succeed completely, or fail (producing no output).

For Scala, however, if a successfuly compilation has already completed, the compiler may cache the reuse the
earlier output, performing an _incremental compilation_. As far as Fury is concerned, this is an implementation
detail, and the only observable difference should be that compilation will sometimes be faster.

Fury users will not typically _see_ the outputs (such as class files) from a compilation in the form of files
on disk. They do exist within Fury's cache, but usually, while developing software it is sufficient just to
know whether compilation succeeded or failed (with error messages), or to see the output from running the
tests. JAR files can be saved to disk from a successful compilation.

Each module must define a compiler, which will be invoked to convert some source files to some outputs, but
different types of modules may have additional behavior. Most modules will be _library_ modules, which do this
and nothing more. _Application_ modules may additionally have a `main` method in one of their objects which is
_run_ after compilation.

This makes application modules suitable for operations which happen at the end of a build, such as running
tests or launching a web server. But they may also run during earlier stages of the build, performing tasks like
source-code generation or bytecode analysis.

_Plugin_ modules may be used to define Scala compiler plugins. Any other module depending on a plugin module
will be compiled with that plugin enabled.

_Benchmark_ modules are similar to application modules, but integrate with
[`jmh`](https://openjdk.java.net/projects/code-tools/jmh/) to instrument the compiled bytecode, and then run the
benchmarks in isolation

#### Application modules

Application modules are a very powerful feature which make it possible to define builds that perform more
varied tasks than simple compliation. Some possibilities include,
- source-code generation
- running tests
- launching a web-server

An `application` module behaves like a `library` module, but will additionally run a `main` method once it has
finished compiling. The class which defines the `main` method must be specified in the module, for example,
```
fury module update --type application --main com.example.Main
```

Parameters to the `main` method can also be supplied, following a `--` argument which separates Fury's arguments
from those to be passed to the application. These may be specified in the module definition, or as parameters to
the build command, for example,
```
fury build run --module <module name> -- arg1 arg2 arg3
```

Application modules are part of the build, and it is often useful to indicate 

##### Determinism

An application module may perform operations which, for a given set of inputs, always produce the same output.
These are called _deterministic_, and Fury knows that it can cache the result of a deterministic application
module without having to run it every time.

The `--deterministic`/`-D` option can be used to indicate whether the result of an application module should
be cached, or recomputed every time.

Note that even if a module is marked as deterministic, if its classpath, source code, or compiler options
change, it will be recomputed.

##### Termination

Application modules may be long-lived processes which the user may need to interrupt, and operating systems
provide a variety of signals which can be used to shutdown a running process, with different degrees of
forcefulness.

If the process is interrupted while it is running, usually by pressing `Ctrl+C`, Fury will send a termination
signal to the process. By default, this will be `SIGTERM`, though some applications may need different signals.
These may be specified with the `--termination`/`-T` option, like so,
```
fury module update --termination SIGKILL
```

#### Benchmarks

Carrying out benchmarking puts some special requirements on the build: in particular, the build should not be
performing any other intensive operations while the benchmarks are being run. Fury provides a special type of
module, `benchmarks`, for this purpose.

Currently the only benchmarking tool supported by Fury is
[jmh](https://openjdk.java.net/projects/code-tools/jmh/), but later versions make offer alternative options.

### Aliases

An _alias_ is a shortcut to running a build on a particular module, and will appear in Fury's command menu
(for the layer it is defined within) to make it easier to run (or just compile) that module.

Aliases are useful for setting up common tasks for the build, such as running unit tests or launching a
web-server.

## Publishing

Fury builds may be shared with other developers, and Fury introduces its own scheme for sharing a layer so
that it may be cloned or imported by someone else. We make the distinction between "sharing" and
"publishing". Sharing makes a layer available to other users on the Internet, provided they know how to refer
to it: a hash is used to refer to the layer, but does nothing to make it easy-to-find, and does not add it to
any catalogs.

Publishing attaches a name of your choice to a shared layer, and optionally adds it to a catalog to make it
discoverable. Publishing requires a third-party catalog service, whose job is to aggregate published layers
and make them available for users to view or search. Different catalog services can choose different criteria
for publishing layers, but Fury comes bootstrapped to use the
[`furore.dev` catalog service](https://furore.dev/catalog), which can be freely used by anyone with a
[GitHub](https://github.com) account. A shared layer hash is an immutable reference to a full specification of
the build, and is intended to produce the same binary outputs today or in ten years' time. A published layer
name, however, is mutable and can refer to different (though hopefully not wildly different) definitions at
different times. You should use the published name to get the latest or best version of a layer, whereas the
shared hash should be used to guarantee repeatability. A layer imported into another will always be stored using
its immutable hash, ensuring that the build is repeatable. However, if a layer is imported using a mutable name,
this is also stored in the layer to help with maintenance: it's easy to check if a newer version of a layer has
become available, and to automatically upgrade to the latest version.

### Versioning

Versioning for Fury layers is automatic, and constrained for simplicity. A layer's version will be a pair of
integers representing the "major" and "minor" versions of that layer. Version numbers are assigned
automatically by the catalog service when a layer is published, and will be strictly in increment of `1` over
the latest major version, or one of the minor versions.

The distinction between major and minor versions of a layer is intended, without it being enforced, to
indicate whether that change represents a change which is compatible with earlier versions of the build or
not: compatible changes should be marked as minor version updates, and incompatible changes as major updates.

When publishing, users are given the choice of specifying that a layer update is minor using the `--minor`
(`-M`) flag to the `fury layer publish` command. By default, publishing a layer with Fury will assume that the
changes are incompatible, and the major version number will be updated.

This is important because Fury makes it easy for maintainers of layers which import another layer to update
the import to the latest *minor* version automatically with just a single command. Updating a layer to a new
major version is a manual operation.

#### Version numbers

Version numbers are associated with every published layer, but are not exposed to users through the user
interface. This is deliberate: often, too much significance is placed on certain version numbers, while the
only significant detail is the order of the layers, and whether the differences between them are breaking or
not.

#### Restrictions

Fury will not allow a layer to be published unless doing so from the previous major or minor version.
Normally, for example, if the user has been working from version `7.16` of a layer, then they will be
permitted to publish version `8.0` or `7.17`. But if version `8.0` or `7.17` has already been published, the
user must fetch the latest version from the catalog service and reconcile any differences, before proceeding
with publishing the new version. This is to ensure that a user does not publish layer which accidentally
overwrites changes introduced since they last checked the layer.

### Rationale

Fury's builds are designed to be _reproducible_. That is to say, the build should produce the same binary
artifacts today as it will in ten years' time. Knowing, deterministically, that the build will behave the same
in the future is hugely beneficial to maintainers.

But this makes it harder to compose builds, that is, to combine two different projects in the same build if
they share _similar_ but _non-identical_ dependencies. While identical dependencies can be trivially shared
without either project needing to deviate from its reproducible definition, if a project transitively depends
upon different versions of the same dependency (which cannot co-exist in the same classpath), one or both
builds must deviate from their reproducible definition in order to accommodate the other.

By giving the maintainer a choice of publishing a _major_ or _minor_ update to a layer, they are able to decide
whether existing users of that layer should receive the update semi-automatically, or whether the update should
require more manual intervention. In practice, users should be able to make "semi-automatic" updates with the
_expectation_ that their build will continue to work; thus, that they can be made without much thought, whereas
"manual" updates, where the major version of an imported layer is changed, should come with the expectation
that additional work may be required to ensure the build continues to work correctly.

The practical difference between a minor and major update to a layer is how (or, how _easily_) and when the
update may be applied. In all cases, an imported layer may be manually updated to a more recent version using,
```
fury layer update --layer <layer-name>
```
which will check the catalog service for the most recent revision of the current major version of the layer,
or,
```
fury layer update --increment --layer <layer-name>
```
which will increment the layer to the most recent revision of the next major version of the layer, or,
```
fury layer update --latest --layer <layer-name>
```
which will update the imported layer to the most recent revision of the highest version number of the layer.

You can view all the versions of a particular layer that are available on catalog service with the command,
```
fury layer revisions
```

The `--recursive`/`-r` option may also be used to update each imported layer to its most recent minor version,
and then to repeat the operation on the imported layers of each, recursively. This should ensure that the layer
is fully updated to the most recent, _compatible_ version.

#### Semi-automatic updates

#### Note on layer compatibility

Within a hierarchy of layers, it remains possible for different major versions of the same layer to coexist.
Fury requires only that the set of projects needed to run a build be coherent, but doesn't enforce this
constraint on layers. However, having fewer different versions of the same layer makes it less likely that there
would be different versions of the same project within the dependency tree, so keeping layers updated to more
recent versions is a good approach to avoiding incoherent builds.

#### Deciding to publish major or minor updates

When publishing a new revision of a layer, the nature of the changes since the previous published version of
that layer will determine whether the update can be a _minor_ update or whether it must be a _major_ update.

The key criterion is whether a project which depended on projects defined in an earlier revision of the layer
will continue to provide the same functionality as 

- adding a source directory?
- removing a source directory?
- adding a module?
- removing an API call?
- adding an API call?
- updating a dependency?


#### Rectifying publishing mistakes

Publishing a layer is very easy with Fury, which also makes it easy to publish a layer which contains a mistake.
While publishing a layer is always final, mistakes can still be rectified. The solution is typically to publish
again.

If a layer is published as a minor update when it should have been a major update—that is, the layer was
incorrectly determined to be backwards compatible—then it is sufficient to publish a new minor version which
_is_ compatible (potentially just reverting back to the previous revision). The incompatible revision will still
exist as a published layer, but Fury will only ever automatically choose the latest version of a layer.

It remains possible, in the time between an erroneous layer being published, and its replacement layer
being published, that one or more developers may import it. The layer may also be fully functional if the error
lies only in its backwards-compatibility. But any such user may run into problems when attempting to update
the layer to a more recent minor version, expecting compatibility. A method of masking incompatible layer revisions is being considered, to (at least) notify users to expect compatibility issues when this happens.

### Control over layer updates

When importing a layer, different strategies may be used to help Fury decide what changes it should make
automatically. The _strategy_ is a property of the _import_, which should be chosen when the layer is imported.
By default, the `auto` strategy will be used, which should be a reasonable strategy for most imports. The other
available strategies are, `manual`, `defer` and `locked`, and others may be added in the future.

#### `locked` imports

If a layer is imported as `locked`, it cannot change in any way. That includes any layers imported transitively
by the locked layer. This is a useful strategy for importing large layers which you do not want to change,
perhaps because you want to retain compatibility with them, such as `scala/ecosystem`. Locked layers cannot be
changed, either automatically by Fury, or manually by the user.

#### `manual` imports

`manual` imports behave the same as `locked` imports, except that they may be modified by the user.

#### 'defer' imports

Imports using the `defer` strategy will be automatically upgraded to use minor revisions that are used by other
`locked` or `manual` imports.

#### 'auto' imports


### Layer references

Fury layers are stored and distributed independently of the source code they build. This approach has the
benefit that a build developer or maintainer does not need write-access to the source code repository in order
to publish a build for it. This works well when users start exploring a project from its Fury layer, perhaps by
cloning the layer or importing it. But many users will start by cloning a Git repository containing source code,
and will only later want to start building that source code using Fury.

This workflow is accommodated by Fury. If the Git repository contains a `.fury.conf` file, it will contain a
reference to the layer file which Fury can automatically fetch and use. In doing so, Fury will automatically use
the latest minor version of the layer referenced in the `.fury.conf` file.

This means that the build associated with a particular Git repository commit may continue to evolve after the
source code has been committed and tagged with a version number. As the build describes all the dependencies
for the projects defined in the layer

#### 

## Catalog services

Publishing Fury layers so that other users can see an up-to-date catalog and access them requires a single
source of truth about the catalog, and the layers it contains. Fury's publishing model assumes that a central
server, accessible over HTTP at a pre-agreed domain (or IP address), will be used to access the current
catalog, and to publish new layers to it. We call this the _catalog service_.

### Resilience

A single source of truth implies, unfortunately, a single point of failure, and it would not be satisfactory to
assume that any server running the catalog service would have 100% uptime, or would be accessible continuously
from any location around the world. Fury, as the only client to the catalog service, will fall back to an
alternative solution if the catalog server is unavailable for whatever reason.

If an HTTP request to the catalog service fails, Fury will attempt to access a DNS record associated with the
catalog service's domain name, which will point to an [IPFS](https://ipfs.io) hash of a recent version of the
layer catalog. DNS is inherently distributed, and IPFS is a peer-to-peer network, so both will continue to
function during while the catalog service is unavailable. It will not be possible to publish a layer during any
period of unavailability, or access the definitive most-recent catalog, but the architecture is designed to
degrade gracefully in the event of failure. Even though _publishing_ a layer will be temporarily impossible,
_sharing_ a layer will continue to work.

### Furore

[Furore](https://furore.dev/) is a catalog service provided by [Propensive O&Uuml;](https://propensive.com/)
for sharing Fury layers, and fresh Fury installations are configured to use Furore as their default catalog
service.

### Custom Catalog Services

It may be desirable for developers to develop and host their own catalog service, accessible publicly as
_Furore_ is, or restricted to corporate intranet. As well as providing control over the visibility of the
catalog service, hosting a custom catalog service enables alternative methods of authentication using
[OAuth](https://en.wikipedia.org/wiki/OAuth) and custom business logic for layer verification and the choice of
public name.

The API a catalog service must implement in order for Fury to use it is quite simple, but is still evolving and
is not yet finalized or published. There are medium-term plans to release the Furore server source code as open
source to allow developers to fork and implement their own services, should they want to.

## Repositories

A Fury layer will probably have one or more Git repositories associated with it, which modules will use to get
source code to compile. Repositories are specified as both a Git URL and a commit hash, meaning that the
reference in the layer will point to exactly the same code for all time. Additionally, a branch or tag may be
associated with the reference too, but this only serves as a pointer to be checked for updates to the source
code, and is not used to determine which commit of the repository to check out and use for the build. Were Fury
to do so, it would make builds non-repeatable, and worse, unacceptably fragile as time passes and branches
change.

### Forking

Whenever Fury runs a build referencing Git repositories containing source code, it will need local copies of
those repositories. Fury will clone and checkout the referenced sources into its cache directory, as necessary.

### Local repositories

Usually, a developer will be actively working on the source code from a repository at the same time as using or
developing a Fury build. The working directory would be the same for both the Fury build and the source
repository.

If Fury detects that its working directory is a Git repository, and furthermore, is one of the repositories
defined in the layer (which Fury will work out from its remote URL), it will use the working directory as a
"forked" version of the repository, instead of checking out a separate repository to Fury's cache.

This means that you can work on the sources for any of the repositories referenced by your current layer, from
the same working directory, and the build will automatically use your current version of the sources for that
repository.

Fury can also automatically check out a Git repository into its current working directory. This is a
special-case of forking, where the repository is will be forked into the current working directory.

#### Forking a repository

To fork a repository, run the command,
```
fury repo fork -r <repo id> -f <destination>
```

This will create a checkout of the repository with the id `<repo id>` into the directory `<destination>`,
creating it if necessary. Both the `-r`/`--repo` and `-f`/`--dir` options are required.

#### Checking out a repository

You can check out a repository into your working directory if you do not already have a repository checked out,
or if the repository you currently have checked out does not have any uncommitted or unpushed changes. This is
because there can be at most one repository checked out in the working directory at a time, so checking out a
new repository requires that the old repository be _checked in_ first, in such a way that the layer can
continue to refer to the same checked-out sources, by means of a remote URL and a commit hash.

If this condition is met, a repository can be checked out with,
```
fury repo checkout -r <repo id>
```

