# Indirect

Indirect is an executable that can be used to indirectly call other executables.
It can be useful when can't (or don't want to) change how something is invoked,
but what finer control over what is actually invoked.

## Motivating Example

We use [Fourmolu][] to format Haskell code in a large team across a number of
projects. How that code is formatted differs slightly from version to version,
so we all have to agree on the version we use -- otherwise two folks working on
the same code over time might repeatedly reformat each others work back and
forth -- causing confusion and frustration each time.

Keeping these versions correct is not trivial. Some editor tooling makes it
difficult to ensure its "format on save" feature calls a given executable, and
almost none make it easy to have that executable differ project to project.

With `indirect` installed and symlinked as the `fourmolu` on `$PATH`, invoking
naively (as editor tooling will do) will consult a configuration file to
determine which actual executable to invoke based on some rules. Additionally,
`indirect` can install the executable if necessary, using the instructions given
in the same configuration file.

## Installation

TODO

## Usage

```console
TODO
```

## Configuration

```toml
# ~/.config/indirect/indirect.toml

[fourmolu]

# Arbitrary vars.x can be defined and accessed in any setting string as ${x}
vars.bin = "~/.local/bin"
vars.name = "fourmolu"
vars.version = "0.16.2.0"

# Required
binary = "${bin}/${name}-${version}"

# Optional. If defined and binary does not exist, will be invoked to install it
install = """
  curl -sSf -L -o ${binary} https://github.com/${name}/${name}/releases/download/v${version}/fourmolu-${version}-linux-x86_64
  install ${binary} ${bin}/${binary}
"""

# Other keys define overrides which inherit by default
some-project.vars.version = "0.13.0.1"

# And expect rules to dictate when their settings will be used
some-project.rules.cwd.matches = "*/some-project"

other-project.version = "0.16.0.0"
other-project.rules.cwd.matches = "*/other-project"
```

### Rules

Rules are made up of objects and operations:

```toml
some-name.rules.{object}.{operation} = {value}
```

| Object | Operation | Value    | Description |
| ---    | ---       | ---      | ---         |
| `cwd`  | `matches` | `<glob>` | Rules is `true` if the current directory matches `<glob>` |

---

`indirect` is licensed AGPLv3. See [COPYING](./COPYING).
