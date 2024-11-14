# Indirect

Indirect is an executable that can be used to indirectly call other executables.
It can be useful when you can't (or don't want to) change how something is
invoked, but want finer control over what is actually invoked.

## Motivating Example

We use [Fourmolu][] to format Haskell code in a large team across a number of
projects. How that code is formatted differs slightly from version to version,
so we all have to agree on the version we use. Otherwise, two folks working on
the same code over time might repeatedly reformat each others work back and
forth, causing confusion and frustration each time.

[fourmolu]: #todo

Keeping these versions correct is not trivial. Some editor tooling makes it
difficult to ensure its "format on save" feature calls a given executable, and
almost none make it easy to have that executable differ project to project.

There are solutions for this. Nix is the hammer that works for all nails, but
this project explores a lighter, more surgical solution.

With `indirect` installed and symlinked as the `fourmolu` on `$PATH`, invoking
it naively (as editor tooling will most often do) will consult a configuration
file to determine which _actual_ executable to invoke.

Additionally, `indirect` can install the executable if necessary, using the
instructions given in the same configuration file.

## Installation

TODO: GitHub releases with macOS/Linux executables.

## Usage

If invoked as an executable whose name is present in the loaded configuration,
that configuration is used to install (if necessary) and then invoke that
executable. Otherwise, there is currently one subcommand, `setup`:

```console
% indirect setup --help
Usage: indirect setup [--only NAME [--only NAME]] [--no-install] 
                      [--links DIRECTORY] [--force]

  Install and link defined executables

Available options:
  --only NAME              Setup only the given executables
  --only NAME              Setup only the given executables
  --no-install             Don't run executable install stanzas
  --links DIRECTORY        Create symbolic links to the indirect executable as
                           DIRECTORY/NAME
  --force                  Create symbolic links even if something exists there
  -h,--help                Show this help text
```

## User Configuration

```toml
# ~/.config/indirect/indirect.toml

# The special name defaults apply to all executables
[defaults]
# Arbitrary vars.x that can be accessed as ${x} in all string settings
vars.bin = "/home/patrick/.local/bin"

# vars can reference other vars, even those not yet defined
vars.artifact = "${name}-${version}-linux-x86_64"

# Required. This is what will actually be called when indirect is invoked as an
# executable named "fourmolu"
binary = "${bin}/${name}-${version}"

# Optional. If defined and binary does not exist, this will be invoked to
# install it
install = """
  cd /tmp
  curl -sSf -L -O https://github.com/${name}/${name}/releases/download/v${version}/${artifact}
  install ${artifact} ${binary}
"""

[fourmolu]
vars.name = "fourmolu"
vars.version = "0.16.2.0"
```

With this configuration in place, run `indirect setup`:

```console
% indirect setup --links ~/.local/bin
[indirect] Installing /home/patrick/.local/bin/fourmolu-0.16.2.0
[indirect] Linking /home/patrick/.local/bin/fourmolu to indirect executable
```

You'll find a symlink from `fourmolu` to `indirect`:

```console
% ls -l ~/.local/bin/fourmolu
lrwxrwxrwx 1 patrick patrick 160 Nov 13 22:20 /home/patrick/.local/bin/fourmolu -> /home/patrick/.local/bin/indirect
```

Running `fourmolu` will use this executable, which is actually `indirect`. When
invoked this way, `indirect` will (install if necessary and) act as a
pass-through to it:

```console
% fourmolu --version
fourmolu 0.16.2.0 e8aa5a666f94eca63e2d8bb1db80b419484ed61a
using ghc-lib-parser 9.10.1.20240511
```

```console
% ls -l ~/.local/bin/fourmolu-*
-rwxr-xr-x 1 patrick patrick 64309880 Nov 13 22:20 /home/patrick/.local/bin/fourmolu-0.16.2.0
```

## Project Configuration

Indirect configurations can be merged, with `.indirect.toml` taking precedence
over the user configuration described above.

This means you can check a `.indirect.toml` file into any project:

```toml
[fourmolu]
vars.version = "0.13.1.0"
```

Invoking `fourmolu` in this directory will now do the _Right Thing_:

```console
% fourmolu --version
[indirect] Installing /home/patrick/.local/bin/fourmolu-0.13.1.0
fourmolu 0.13.1.0 9181f7e5daf4fe816adf69cdaf5c0c76dcd0a089
using ghc-lib-parser 9.6.2.20230523

% fourmolu --version
fourmolu 0.13.1.0 9181f7e5daf4fe816adf69cdaf5c0c76dcd0a089
using ghc-lib-parser 9.6.2.20230523
```

```console
% ls -l ~/.local/bin/fourmolu-*
-rwxr-xr-x 1 patrick patrick 59604696 Nov 13 22:34 /home/patrick/.local/bin/fourmolu-0.13.1.0
-rwxr-xr-x 1 patrick patrick 64309880 Nov 13 22:20 /home/patrick/.local/bin/fourmolu-0.16.2.0
```

---

`indirect` is licensed AGPLv3. See [COPYING](./COPYING).
