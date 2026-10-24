# Installation

MeowMeow can be built from source with `rebar3`, or run as a container image.
This page covers the source build. For containers see [Docker](docker.md).

## Requirements

| Requirement | Version | Notes |
|-------------|---------|-------|
| Erlang/OTP | 27 or newer, **28 recommended** | The server uses the `socket` and `json` modules that ship with modern OTP. CI builds on OTP 28. |
| rebar3 | 3.x | Any recent release. |
| git | any | Optional. Used at compile time to embed the commit hash into the `Server` header. |

No third-party Erlang dependencies are needed; `rebar.lock` is empty on purpose.

## Build

Clone the repository and compile:

```bash
git clone https://github.com/0xf104a/MeowMeow.git
cd MeowMeow
rebar3 compile
```

A pre-compile hook (`scripts/git_vsn.sh`) writes `include/git_vsn.hrl` with
the current commit hash, which becomes part of the version string sent in the
`Server` response header (for example `MeowMeow/2.0-alpha10.d98e0ab`, or
`MeowMeow/2.0-alpha10.d98e0ab.dirty` with uncommitted changes). If the source
tree is not a git checkout the hash is reported as `unknown`, unless it is
given in the `GIT_SHA` environment variable as the container builds do.

## Build a release

For deployment, build a self-contained OTP release that bundles the Erlang
runtime:

```bash
rebar3 as prod release
```

The release is written to `_build/prod/rel/MeowMeow/`. It can be copied to any
machine with the same operating system and architecture; Erlang does not need
to be installed there.

The `default` profile (`rebar3 release`) produces a development release in
`_build/default/rel/MeowMeow/` that symlinks to the build tree and does not
include the runtime.

## Install configuration files

The paths of the configuration files are compiled into the binary. The
defaults, defined in `src/config.hrl`, are:

| File | Default path | Purpose |
|------|--------------|---------|
| Server configuration | `/etc/nya/meow.conf` | Listening address, logging, limits, module list. |
| Routing configuration | `/etc/nya/routes.conf` | Request handling rules. |

Create the directory and copy the sample configuration:

```bash
sudo mkdir -p /etc/nya
sudo cp config/meow.conf config/routes.conf config/mimes.conf /etc/nya/
```

The sample `routes.conf` includes `mimes.conf` by an absolute path. Open
`/etc/nya/routes.conf` and make sure the `Include` line points at the copy you
just made:

```text
Include "/etc/nya/mimes.conf"
```

!!! tip "Using a different directory"
    Change `accessfile` and `configfile` in `src/config.hrl` and rebuild:

    ```erlang
    -define(accessfile, "/etc/nya/routes.conf").
    -define(configfile, "/etc/nya/meow.conf").
    ```

## Run

Start the release in the foreground:

```bash
./_build/prod/rel/MeowMeow/bin/MeowMeow foreground
```

The start script is a standard `relx` extended start script. Run it without
arguments to see every mode; the most useful ones are:

| Command | Behaviour |
|---------|-----------|
| `MeowMeow foreground` | Run attached to the terminal. Use this under systemd or Docker. |
| `MeowMeow daemon` | Detach and run in the background. |
| `MeowMeow console` | Run with an interactive Erlang shell attached. |
| `MeowMeow remote_console` | Attach a shell to a running daemon. |
| `MeowMeow stop` | Stop a running daemon. |

On start MeowMeow prints a banner with its version, loads the configuration,
loads the modules listed in `LoadModules`, parses the routing file and starts
listening. If the routing file cannot be parsed the server logs the reason and
exits with status `255` instead of running with a partial configuration.

## Development mode

During development the quickest loop is the rebar3 shell, which compiles the
project and starts the application with the server attached to your terminal:

```bash
rebar3 shell
```

Set `LogLevel 4` in `meow.conf` to see every request and rule evaluation.
