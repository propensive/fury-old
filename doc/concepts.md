# Concepts

## Basic concepts

Fury builds are modeled as _layers_ which are the atoms of distribution. They may define _projects_ and/or inherit them from imported layers. Projects consist of _modules_, which may depend on other modules forming a directed acyclic graph which can span projects and layers. Modules compile _sources_ which are taken from a _repo_ (repository) using a compiler (which is just another module).

## Build structure

### Layers

#### Layer Imports

#### Navigating Layers

Layers form a tree

### Projects

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

#### Benchmarks

Carrying out benchmarking puts some special requirements on the build: in particular, the build should not be performing any other intensive operations while the benchmarks are being run. Fury provides a special type of module, `benchmarks`, for this purpose.

Currently the only benchmarking tool supported by Fury is [jmh](https://openjdk.java.net/projects/code-tools/jmh/), but later versions make offer alternative options.

## Publishing

Fury builds may be shared with other developers, and Fury introduces its own scheme for sharing a layer so
that it may be cloned or imported by someone else. We make the distinction between "sharing" and
"publishing". Sharing makes a layer available to other users on the Internet, provided they know how to refer
to it: a hash is used to refer to the layer, but does nothing to make it easy-to-find, and does not add it to
any catalogs.

Publishing attaches a name of your choice to a shared layer, and optionally adds it to a catalog to make it
discoverable. Publishing requires a third-party catalog service, whose job is to aggregate published layers
and make them available for users to view or search. Different catalog services can choose different criteria
for publishing layers, but Fury comes bootstrapped to use the `furore.dev` catalog service, which can be
freely used by anyone with a GitHub account. A shared layer hash is an immutable reference to a full
specification of the build, and is intended to produce the same binary outputs today or in ten years' time. A
published layer name, however, is mutable and can refer to different (though hopefully not wildly different)
definitions at different times. You should use the published name to get the latest or best version of a
layer, whereas the shared hash should be used to guarantee repeatability. A layer imported into another will
always be stored using its immutable hash, ensuring that the build is repeatable. However, if a layer is
imported using a mutable name, this is also stored in the layer to help with maintenance: it's easy to check
if a newer version of a layer has become available, and to automatically upgrade to the latest version.

### Versioning

Versioning for Fury layers is opaque, automatic, and constrained for simplicity. A layer's version will be a
pair of integers representing the "major" and "minor" versions of that layer. Version numbers are
assigned automatically by the catalog service when a layer is published, and will be strictly in increment
of `1` over the latest major version, or one of the minor versions.

The distinction between major and minor versions of a layer is intended, without it being enforced, to
indicate whether that change represents a change which is compatible with earlier versions of the build or
not: compatible changes should be marked as minor version updates, and incompatible changes as major updates.

When publishing, users are given the choice of specifying that a layer update is minor using the `--minor` (`-M`) flag to the `fury layer publish` command. By default, publishing a layer with Fury will assume that the changes are incompatible, and the major version number will be updated.

This is important because Fury makes it easy for maintainers of layers which import another layer to update
the import to the latest *minor* version automatically with just a single command. Updating a layer to a new major version
is a manual operation.

#### Version numbers

Version numbers are associated with every published layer, but are not exposed to users through the user
interface. This is deliberate: often, too much significance is placed on certain version numbers, while all
Fury needs to care about is the order of the layers, and whether the differences between them are breaking or
not.

#### Restrictions

Fury will not allow a layer to be published unless doing so from the previous major or minor version.
Normally, for example, if the user has been working from version `7.16` of a layer, then they will be
permitted to publish version `8.0` or `7.17`. But if version `8.0` or `7.17` has already been published, the
user must first fetch the latest version from the catalog service and reconcile any differences, before

### Layer references

Fury layers are stored and distributed independently of the source code they build. This approach has the
benefit that abuild developer or maintainer does not need write-access to the source code repository in order to publish
a build for it. This works well when users start exploring a project from its Fury layer, perhaps by cloning
the layer or importing it. But many users will start by cloning a Git repository containing source code, and
will only later want to start building that source code using Fury.

This workflow is accommodated by Fury. If the Git repository contains a `.fury.conf` file, it will contain a
reference to the layer file which Fury can automatically fetch and use. In doing so, Fury will automatically
use the latest minor version of the layer referenced in the `.fury.conf` file.

This means that the build associated with a particular Git repository commit may continue to evolve after the source code has been committed and tagged with a version number. As the build describes all the dependencies for the projects defined in the layer

That means that 

#### 

## Catalog services

Publishing Fury layers so that other users can see an up-to-date catalog and access them requires a single source of truth about the catalog, and the layers it contains. Fury's publishing model assumes that a central server, accessible over HTTP at a pre-agreed domain (or IP address), will be used to access the current catalog, and to publish new layers to it. We call this the _catalog service_.

### Resilience

A single source of truth implies, unfortunately, a single point of failure, and it would not be satisfactory to assume that any server running the catalog service would have 100% uptime, or would be accessible continuously from any location around the world. Fury, as the only client to the catalog service, will fall back to an alternative solution if the catalog server is unavailable for whatever reason.

If an HTTP request to the catalog service fails, Fury will attempt to access a DNS record associated with the catalog service's domain name, which will point to an IPFS hash of a recent version of the layer catalog. DNS is inherently distributed, and IPFS is a peer-to-peer network, so both will continue to function during while the catalog service is unavailable. It will not be possible to publish a layer during any period of unavailability, or access the definitive most-recent catalog, but the architecture is designed to degrade gracefully in the event of failure. Even though _publishing_ a layer will be temporarily impossible, _sharing_ a layer will continue to work.

### Furore

[Furore](https://furore.dev/) is a catalog service provided by [Propensive O&Uuml;](https://propensive.com/) for sharing Fury layers, and fresh Fury installations are configured to use Furore as their default catalog service.

### Custom Catalog Services

It may be desirable for developers to develop and host their own catalog service, accessible publicly as _Furore_ is, or restricted to corporate intranet. As well as providing control over the visibility of the catalog service, hosting a custom catalog service enables alternative methods of authentication using [OAuth](https://en.wikipedia.org/wiki/OAuth) and custom business logic for layer verification and the choice of public name.

The API a catalog service must implement in order for Fury to use it is quite simple, but is still evolving and is not yet finalized or published. There are medium-term plans to release the Furore server source code as open source to allow developers to fork and implement their own services, should they want to.

## Repositories

A Fury layer will probably have one or more Git repositories associated with it, which modules will use to
get source code to compile. Repositories are specified as both a Git URL and a commit hash, meaning that the reference in the layer will point to exactly the same code for all time. Additionally, a
branch or tag may be associated with the reference too, but this only serves as a pointer to be checked for
updates to the source code, and is not used to determine which commit of the repository to check out and use
for the build. Were Fury to do so, it would make builds non-repeatable, and worse, unacceptably fragile as
time passes and branches change.

### Forking

Whenever Fury runs a build referencing Git repositories containing source code, it will need local copies of those repositories. Fury will clone and checkout the referenced sources into its cache directory, as necessary.

### Local repositories

Usually, a developer would be actively working on the source code from a repository at the same time as using or developing a Fury build. The working directory would be the same for both the Fury build and the source repository.

If Fury detects that its working directory is a Git repository, and furthermore, is one of the repositories defined in the layer (which Fury will work out from its remote URL), it will use the working directory as a "forked" version of the repository, instead of checking out a separate repository to Fury's cache.


