# Installing Fury

## Linux and Mac OS X

Fury can be installed on Linux and MacOS X. To install the latest release of Fury, run,
```sh
curl -Ls fury.build | sh
```
from a terminal.

You may prefer to inspect the installer script before executing it (it's quite short and readable), like so:
```sh
curl -Ls fury.build > install
cat install
./install
```

## Windows

Fury can be run on Windows using the
[Windows Subsystem for Linux 2](https://docs.microsoft.com/en-us/windows/wsl/about) (WSL2).

If you have not already done so, [install WSL2](https://docs.microsoft.com/en-us/windows/wsl/install-win10).
From your WSL2 terminal, it should then be possible to follow the instructions above for installing on Linux.

Alex Ioffe has kindly produced some instructional videos on setting up WSL2 for Scala development under
Windows:
- [Part 1](https://www.youtube.com/watch?v=JBAQPS9MYkY): "Baby Steps"
- [Part 2](https://www.youtube.com/watch?v=uM4Ay1xHKJY): "Enlightenment"

These videos make no reference to Fury, but are useful viewing for understanding WSL2.

## Installing from a launcher script

Git repositories which use Fury may also choose to bundle a small launcher script to make bootstrapping easier.
This is usually a script called `fury` in the root directory. This script can be used to run Fury without
making changes to your system, but also supports installation of Fury with the command
```sh
./fury system install
```

Both approaches will attempt to install Fury for the current user, unless it is run as `root`, in which case
Fury will be installed for all users. The installation uses standard XDG directory locations, with
modifications made to the user's shell startup scripts (`~/.bashrc` and/or `~/.zshrc`) to ensure the `fury`
command is on the path.

If an older version of Fury is already running, it will need to be manually stopped with,
```sh
fury stop
```
before the new version can be used, but any subsequent calls to `fury` will start the newly-installed version.

The installation script can be run more than once.

## Uninstallation

Fury can be manually uninstalled by,
- removing the `~/.local/share/fury` directory,
- removing the `~/.cache/fury` directory,
- removing the `~/.config/fury` directory, and,
- deleting all the lines in `~/.bashrc` and `~/.zshrc` which contain the string `# Added by Fury`
