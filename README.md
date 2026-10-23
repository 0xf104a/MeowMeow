# MeowMeow

[![Erlang CI](https://github.com/0xf104a/MeowMeow/actions/workflows/erlang.yml/badge.svg?branch=master)](https://github.com/0xf104a/MeowMeow/actions/workflows/erlang.yml)
[![Tests](https://github.com/0xf104a/MeowMeow/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/0xf104a/MeowMeow/actions/workflows/test.yml)
[![Docker](https://github.com/0xf104a/MeowMeow/actions/workflows/docker-publish.yml/badge.svg)](https://github.com/0xf104a/MeowMeow/actions/workflows/docker-publish.yml)
[![Docs](https://github.com/0xf104a/MeowMeow/actions/workflows/docs.yml/badge.svg?branch=master)](https://github.com/0xf104a/MeowMeow/actions/workflows/docs.yml)
[![Documentation](https://readthedocs.org/projects/meowmeow/badge/?version=latest)](https://meowmeow.readthedocs.io/en/latest/)
[![Erlang/OTP](https://img.shields.io/badge/Erlang%2FOTP-27%2B-A90533?logo=erlang&logoColor=white)](https://www.erlang.org/)
[![Dependencies](https://img.shields.io/badge/dependencies-0-brightgreen)](https://github.com/0xf104a/MeowMeow/blob/master/rebar.config)
[![Release](https://img.shields.io/github/v/tag/0xf104a/MeowMeow?label=release)](https://github.com/0xf104a/MeowMeow/tags)
[![License](https://img.shields.io/github/license/0xf104a/MeowMeow)](https://github.com/0xf104a/MeowMeow/blob/master/LICENSE)

A tiny, modular HTTP/1.1 web server written in Erlang/OTP, with a gateway
that exposes stdio [Model Context Protocol](https://modelcontextprotocol.io/)
servers over HTTP. It serves static files out of the box, is configured
through two small text files, and grows through *modules* that add new
directives to its routing language.

```text
Route /generate_204
 No-Content
End

Route /mcp
 MCP-Tool-Streamable "/opt/tools/bin/weather-mcp" 60000
End

Route *
 Set-Header X-Powered-By "Pusheen the Cat"
 DocDir /var/www/html
End
```

**Documentation:** <https://meowmeow.readthedocs.io/> (also in [`docs/`](https://github.com/0xf104a/MeowMeow/blob/master/docs/index.md)).

## Features

- **Dependency free.** Only `kernel`, `stdlib` and `crypto` from OTP. No third-party Erlang libraries, no `rebar.lock` entries.
- **One process pair per connection.** Each client gets its own handler and TCP flow-control process, so a slow client or a long download never blocks the others.
- **Declarative routing.** A request runs through an ordered list of rules. Rules nest under conditions on the path, the `Host` header, any other header, or an environment variable.
- **Modules.** Every directive beyond the five basic ones comes from a module. Writing one means implementing one small Erlang behaviour.
- **MCP gateway.** The `mcp` module runs a stdio JSON-RPC tool per session and speaks the Streamable HTTP transport (`2025-11-25`), the legacy SSE transport (`2024-11-05`), or a single-session SSE variant for simple clients.
- **Streaming static files.** Files are sent in 64 KiB chunks; the test suite moves a one-gigabyte file.
- **Container ready.** Multi-stage `Dockerfile`, images on `ghcr.io` signed with cosign, and a containerised integration suite.

## Quick start

Requires Erlang/OTP 27 or newer (28 recommended) and `rebar3`.

```bash
git clone https://github.com/0xf104a/MeowMeow.git
cd MeowMeow
rebar3 as prod release
```

The server reads `/etc/nya/meow.conf` and `/etc/nya/routes.conf` (paths are
set in `src/config.hrl`). Install the sample configuration and point its
`Include` at the right place:

```bash
sudo mkdir -p /etc/nya /var/www/html
sudo cp config/meow.conf config/routes.conf config/mimes.conf /etc/nya/
sudo sed -i 's#/etc/MeowMeow/#/etc/nya/#' /etc/nya/routes.conf
echo '<h1>Nya~ It works!</h1>' | sudo tee /var/www/html/index.html
```

Edit `/etc/nya/meow.conf` if you want another port than `80`, then run:

```bash
./_build/prod/rel/MeowMeow/bin/MeowMeow foreground
curl -i http://127.0.0.1/
```

For development, `rebar3 shell` compiles and starts the server attached to
your terminal. Set `LogLevel 4` to see every rule evaluation.

Full walkthrough: [Installation](https://github.com/0xf104a/MeowMeow/blob/master/docs/getting-started/installation.md)
and [Quickstart](https://github.com/0xf104a/MeowMeow/blob/master/docs/getting-started/quickstart.md).

## Configuration

Two files, read once at startup:

| File | Purpose | Documentation |
|------|---------|---------------|
| `meow.conf` | How the server runs: `ListenHost`, `ListenPort`, `LogLevel`, `KeepAlive`, `AllowLegacyHttp`, `MaxPostSize`, `LoadModules`. | [Server](https://github.com/0xf104a/MeowMeow/blob/master/docs/configuration/server.md) |
| `routes.conf` | What happens to each request: rules, conditional blocks, includes. | [Routes](https://github.com/0xf104a/MeowMeow/blob/master/docs/configuration/routes.md) |

```text
# meow.conf
ListenHost 0.0.0.0
ListenPort 80
LogLevel 3
KeepAlive 10000
LoadModules static,header,route,env
```

```text
# routes.conf
Include "/etc/nya/mimes.conf"      # Content-Type by extension

Route *
 Set-Header X-Powered-By "Pusheen the Cat"
End

Host "admin.*"                     # conditions nest
 Route /secret*
  Disallow
 End
End

If-Env MAINTENANCE on              # react to the environment
 Set-Code 503
 Send-File /var/www/maintenance.html
End

Route *
 DocDir /var/www/html              # terminal: sends and stops
End
```

Rules run top to bottom. `Set-Header` and `Set-Code` change the response in
progress; *terminal* rules (`DocDir`, `Send-File`, `Abort`, `No-Content`,
`Disallow`, `MCP-Tool-*`) send it and stop, so order matters and the
catch-all goes last.

> [!WARNING]
> Unknown directives abort startup, but structural mistakes such as a missing
> `End` are not detected at load time. Keep `LogLevel 4` on while editing
> routes. See [Configuration overview](https://github.com/0xf104a/MeowMeow/blob/master/docs/configuration/overview.md).

## Modules

| Module | Directives | Documentation |
|--------|------------|---------------|
| `static` | `DocDir`, `Send-File` | [static](https://github.com/0xf104a/MeowMeow/blob/master/docs/modules/static.md) |
| `route` | `Route` | [route](https://github.com/0xf104a/MeowMeow/blob/master/docs/modules/route.md) |
| `header` | `Host`, `If-Header` | [header](https://github.com/0xf104a/MeowMeow/blob/master/docs/modules/header.md) |
| `env` | `If-Env`, `If-Not-Env`, `If-Env-Def` | [env](https://github.com/0xf104a/MeowMeow/blob/master/docs/modules/env.md) |
| `mcp` | `MCP-Tool-Streamable`, `MCP-Tool-SSE`, `MCP-Tool-SSE-Single` | [mcp](https://github.com/0xf104a/MeowMeow/blob/master/docs/modules/mcp.md) |

Load them with `LoadModules static,header,route,env,mcp`. To add your own,
see [Writing modules](https://github.com/0xf104a/MeowMeow/blob/master/docs/development/writing-modules.md).
The complete list of directives is in the
[directive reference](https://github.com/0xf104a/MeowMeow/blob/master/docs/reference/directives.md).

## MCP gateway

Expose any stdio MCP server, authenticated with a shared secret:

```text
LoadModules static,header,route,env,mcp     # meow.conf
MaxPostSize 1048576
```

```text
Route /mcp                                  # routes.conf
 If-Header Authorization "Bearer s3cret"
  MCP-Tool-Streamable "/opt/tools/bin/weather-mcp" 60000
 End
 Abort 401
End
```

An `initialize` `POST` starts the tool and returns a `Mcp-Session-Id`;
further requests, notifications, the `GET` event stream and `DELETE` follow
the Streamable HTTP specification. Idle sessions are terminated after the
configured 60 seconds. Transport details, the SSE variants and the status
codes are in the [mcp module documentation](https://github.com/0xf104a/MeowMeow/blob/master/docs/modules/mcp.md).

## Docker

```bash
docker pull ghcr.io/0xf104a/meowmeow:2.0-alpha10
docker run --rm -p 8080:80 \
  -v "$PWD/my-config:/etc/nya:ro" \
  -v "$PWD/site:/var/www:ro" \
  ghcr.io/0xf104a/meowmeow:2.0-alpha10
```

Images are built from the `Dockerfile` on every version tag and signed with
cosign. See [Docker](https://github.com/0xf104a/MeowMeow/blob/master/docs/getting-started/docker.md)
for building locally and verifying signatures.

## Development

```bash
rebar3 compile        # build
rebar3 shell          # run with a shell attached
rebar3 eunit          # unit tests (one suite per module under test/)
./test.sh             # unit tests + containerised E2E suite, both in Docker
```

Without a local Erlang (e.g. inside a Flatpak IDE) the unit tests run in Docker:

```bash
docker run --rm -v "$PWD":/src:ro -w /build erlang:28.4.1 sh -c 'cp -r /src/. . && rebar3 eunit'
```

Every test title states the expected behaviour and its source (README, RFC, MCP
specification or the function's own documentation); titles with a
`(regression: ...)` note describe a defect the test was written against. In CI
the E2E job only runs once the EUnit job has passed.

The documentation site is built with [Zensical](https://zensical.org/) from
`README.md` and `docs/`:

```bash
pip install -r docs/requirements.txt
python .github/scripts/stage_docs.py && zensical serve
```

- [Architecture](https://github.com/0xf104a/MeowMeow/blob/master/docs/development/architecture.md): processes, ETS tables and the life of a request.
- [Testing](https://github.com/0xf104a/MeowMeow/blob/master/docs/development/testing.md): the test image and the CI workflows.
- [Releasing](https://github.com/0xf104a/MeowMeow/blob/master/docs/development/releasing.md): version bumps, tags and image publishing.

Bug reports and pull requests are welcome on
[GitHub](https://github.com/0xf104a/MeowMeow/issues).

## Status

MeowMeow 2.0 is in alpha. The configuration language and the module
behaviour are usable but may still change between alpha releases; there is no
TLS support yet (terminate it in a reverse proxy), and configuration is
reloaded by restarting the server.

## License

MIT. Copyright (c) 2018-2021, 2026 Anna-Sofia Kasierocka. See
[LICENSE](https://github.com/0xf104a/MeowMeow/blob/master/LICENSE).
