# Modules

Out of the box MeowMeow knows only the five basic rules (`Set-Header`,
`Set-Code`, `Abort`, `No-Content`, `Disallow`). Everything else, including
serving files and matching paths, comes from **modules**: Erlang modules that
implement the `nya_module` behaviour and register directives when the server
starts.

## Loading modules

List the modules to load in `meow.conf`:

```text
LoadModules static,header,route,env
```

They are loaded in order at startup, before `routes.conf` is parsed. Each
module's directives become valid in `routes.conf` only once the module is
loaded; an unloaded directive is an unknown one and aborts startup. The
example above is the minimum for an ordinary static site: `static` to serve
files, `route` for `Route`, `header` for `Host`, and `env` for configuration
that depends on the environment.

## Shipped modules

| Module | Provides | Kind | Page |
|--------|----------|------|------|
| `static` | `DocDir`, `Send-File` | rules (terminal) | [static](static.md) |
| `route` | `Route` | context rule | [route](route.md) |
| `header` | `Host`, `If-Header` | context rules | [header](header.md) |
| `env` | `If-Env`, `If-Not-Env`, `If-Env-Def` | context rules | [env](env.md) |
| `mcp` | `MCP-Tool-Streamable`, `MCP-Tool-SSE`, `MCP-Tool-SSE-Single` | rules (terminal) | [mcp](mcp.md) |

Modules live under `src/modules/<name>/` and are compiled into the release
together with the core, so loading one costs nothing beyond the directives it
registers.

## Two kinds of directives

A module can register two kinds of directives:

**Rules**
:   Act on the response. A rule receives its arguments and the response in
    progress and returns either an updated response (evaluation continues) or
    a final result (evaluation stops). Rules that stop evaluation are called
    *terminal*.

**Context rules**
:   Decide whether a block applies. A context rule receives its arguments and
    the request and returns `true` or `false`. It opens a block that ends with
    `End`.

The exact contract is described in [Writing modules](../development/writing-modules.md).
