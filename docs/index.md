# MeowMeow documentation

The [README](../README.md) gives the overview, the badges and a five-minute quick start. The pages here go into
detail: how to install and run the server, every directive of its two configuration files, what each module
provides, and how the server works inside for anyone who wants to extend it.

## Getting started

| Page | What it covers |
|------|----------------|
| [Installation](getting-started/installation.md) | Requirements, building with rebar3, building a release, where the configuration files live, the start script modes. |
| [Quickstart](getting-started/quickstart.md) | A static site served in five steps, introducing the routing language on the way. |
| [Docker](getting-started/docker.md) | The published images, building locally, mounting configuration, verifying signatures. |

## Configuration

| Page | What it covers |
|------|----------------|
| [Overview](configuration/overview.md) | The two files, their shared syntax, quoting, comments, what happens on errors. |
| [Server](configuration/server.md) | Every directive of `meow.conf`: listening, logging, limits, modules. |
| [Routes](configuration/routes.md) | The rule language of `routes.conf`: rules, context blocks, terminal rules, includes, evaluation order. |
| [Examples](configuration/examples.md) | Complete configurations for common setups. |

## Modules

| Page | What it covers |
|------|----------------|
| [Modules](modules/index.md) | What a module is, how to load one, which ship with the server. |
| [static](modules/static.md) | `DocDir` and `Send-File`. |
| [route](modules/route.md) | `Route` path matching. |
| [header](modules/header.md) | `Host` and `If-Header` conditions. |
| [env](modules/env.md) | `If-Env`, `If-Not-Env` and `If-Env-Def` conditions. |
| [mcp](modules/mcp.md) | Exposing a stdio MCP tool over Streamable HTTP or SSE. |

## Development

| Page | What it covers |
|------|----------------|
| [Architecture](development/architecture.md) | Processes, ETS tables and the life of a request. |
| [Writing modules](development/writing-modules.md) | The `nya_module` behaviour and the rule contract. |
| [Testing](development/testing.md) | The containerised integration suite and CI. |
| [Benchmarking](development/benchmarking.md) | The wrk harness in `bench/` and how to compare revisions. |
| [Releasing](development/releasing.md) | Version bumps, tags and image publishing. |

## Reference

| Page | What it covers |
|------|----------------|
| [Directives](reference/directives.md) | Every directive on one page. |
| [Status codes](reference/status-codes.md) | Which HTTP status the server sends in which situation. |
