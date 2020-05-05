<img src="doc/logo/render_github.png" alt="Fury">

![Build](https://github.com/propensive/fury/workflows/Build/badge.svg?branch=master)

### About
Fury is an experimental dependency manager and build tool for Scala. It is
still under active development.

### Distinct features
* Include source repositories (e. g. on Github) as managed dependencies for your project
* Uses [Bloop](https://scalacenter.github.io/bloop) to compile Scala and Java code in a fast and efficient way
* Share and reuse project definitions through [IPFS](https://ipfs.io)

### Requirements
* Java Development Kit
    * The officially supported version is JDK 8.
    * JDK 11 should work as well. 
    * Other versions, including newer ones, are not supported.
* Bloop
    * If Fury cannot detect a running Bloop server, it will download and launch an own instance.
* Git
* IPFS
    * Supported versions are 0.4.x. IPFS 0.5.x is not supported yet.
    * If Fury cannot detect a running IPFS daemon, a new one will be started. 
    * Fury will also download the IPFS distribution if an installed one cannot be found.
* Tab completion for shell commands works only in Zsh. 
    * If you're a new user, it is strongly recommended to use Fury with Zsh and make use of the command completion,
    * Support for Bash and [Fish](https://fishshell.com) may appear in the future.
    
### Installing latest version from source
```
git clone https://github.com/propensive/fury.git
cd fury
make clean
make install
```

#### Checking if it works
* Run the integration tests: `etc/integration`
* Build Fury itself: `fury`

### Website
Full information about Fury is available on the [Fury website](https://fury.build/).
