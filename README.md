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

[fourmolu]: https://fourmolu.github.io/

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

```console
curl --proto '=https' --tlsv1.2 -sSf \
  https://raw.githubusercontent.com/pbrisbin/indirect/main/install |
  sh -s -- -p ~/.local
```

Or download the appropriate binary from the [latest release][releases], make it
executable, and add it to `$PATH`.

[releases]: https://github.com/pbrisbin/indirect/releases

## User Configuration

Indirect follows [XDG][], meaning user-configuration most likely occurs in
`~/.config/indirect/indirect.toml`.

[xdg]: https://specifications.freedesktop.org/basedir-spec/latest/

This file should be a list of [TOML tables][toml-table]. The table's name
denotes an executable you want `indirect` to manage.

> [!IMPORTANT]
> The special table name `defaults`, if present, will be used as a base for all
> other defined tables.

[toml-table]: https://toml.io/en/v1.0.0#table

Valid keys in any table are:

- `vars.x`: any arbitrary `x`, which can then be accessed as `${x}` in the
  values of other fields

  `vars` can reference other `vars`, even those yet to be defined. All
  environment variables, `name`, and `binary` are also made available for
  interpolation.

- `binary`: required, relative path to name the versioned executable. Typically,
  this would be `${name}-${version}` using a `vars.version` you define.

- `install`: required, if `binary` is not present, this will be executed with
  `sh -c` to install it.

  The desired destination of the binary will be given as `$1`; if running this
  script does not produce an executable at that location, `indirect` will error.

### Example

```toml
[defaults]
vars.artifact = "${name}-${version}-linux-x86_64"

binary = "${name}-${version}"

install = """
  curl -sSf -L -O https://github.com/${name}/${name}/releases/download/v${version}/${artifact}
  install ${artifact} "$1"
"""

[fourmolu]
vars.version = "0.16.2.0"
```

This configuration alone establishes a managed `fourmolu-0.16.2.0` on your
system. To use this installation, you only need to execute `indirect` as a
symlink named `fourmolu`.

Setting up that symlink (or copy, or hardlink) can be done manually, or you can
use our own `setup` sub-command:

```console
% indirect setup --no-install
[indirect] Linking fourmolu to /home/patrick/.local/bin/indirect
```

> [!NOTE]
> `--no-install` is used here so we can witness on-demand installation later.
> Run `indirect setup --help` for more details.

```console
% ls -l ~/.local/bin/fourmolu
lrwxrwxrwx 1 patrick patrick 160 Nov 13 22:20 /home/patrick/.local/bin/fourmolu -> indirect
```

Running `fourmolu` installs the configured version:

```console
% fourmolu --version
[indirect] Installing /home/patrick/.local/share/indirect/targets/fourmolu-0.16.2.0
fourmolu 0.16.2.0 e8aa5a666f94eca63e2d8bb1db80b419484ed61a
using ghc-lib-parser 9.10.1.20240511
```

```console
% ls -l ~/.local/share/indirect/targets/fourmolu-*
-rwxr-xr-x 1 patrick patrick 64309880 Nov 13 22:20 /home/patrick/.local/share/indirect/targets/fourmolu-0.16.2.0
```

Running `fourmolu` again uses what's already there:

```console
% fourmolu --version
fourmolu 0.16.2.0 e8aa5a666f94eca63e2d8bb1db80b419484ed61a
using ghc-lib-parser 9.10.1.20240511
```

## Project Configuration

This is already quite valuable, but how do we deal with a project that expects a
different version? Well, Indirect configurations can be merged, and
`.indirect.toml`, if present, will take precedence over the user configuration
described above.

This means you can check a `.indirect.toml` file into any project and use it to
override some `vars`:

```toml
[fourmolu]
vars.version = "0.13.1.0"
```

Running `fourmolu` in this directory installs the configured version:

```console
% fourmolu --version
[indirect] Installing /home/patrick/.local/bin/fourmolu-0.13.1.0
fourmolu 0.13.1.0 9181f7e5daf4fe816adf69cdaf5c0c76dcd0a089
using ghc-lib-parser 9.6.2.20230523
```

```console
% ls -l ~/.local/share/indirect/fourmolu-*
-rwxr-xr-x 1 patrick patrick 59604696 Nov 13 22:34 /home/patrick/.local/share/indirect/targets/fourmolu-0.13.1.0
-rwxr-xr-x 1 patrick patrick 64309880 Nov 13 22:20 /home/patrick/.local/share/indirect/targets/fourmolu-0.16.2.0
```

And again, running it again uses what's already there:

```console
% fourmolu --version
fourmolu 0.13.1.0 9181f7e5daf4fe816adf69cdaf5c0c76dcd0a089
using ghc-lib-parser 9.6.2.20230523
```

## Development

When developing, be sure to use `--copy-bins` and test by running the installed
`indirect` and never `stack exec indirect`. The latter will infer a directory
within `.stack-work` as the place to look for or install links, which you most
likely do not want.

---

Indirect is licensed AGPLv3. See [COPYING](./COPYING).
