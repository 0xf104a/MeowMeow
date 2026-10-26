# Routing configuration (`routes.conf`)

`routes.conf` is a program that MeowMeow runs for every request. It consists
of *rules* that change or send the response, grouped under *context rules*
that decide whether a group applies. Rules are evaluated from top to bottom,
and the first rule that sends a response ends the evaluation.

```text
Include "/etc/nya/mimes.conf"

Route *
 Set-Header X-Powered-By "Pusheen the Cat"
End

Route /generate_204
 No-Content
End

Host "admin.*"
 Route /secret*
  Disallow
 End
End

Route *
 DocDir /var/www/html
End
```

## How a request is evaluated

1. The response starts as `404 Not Found` with an empty header set.
2. The rule list is walked in file order. A **context rule** (`Route`, `Host`, `If-Header`, `If-Env`, ...) evaluates its condition against the request. If it holds, the rules inside its block are evaluated in place; if not, the whole block is skipped. Blocks nest to any depth, and an inner block is only reached when every enclosing condition holds.
3. An **ordinary rule** (`Set-Header`, `Set-Code`, ...) modifies the response in progress and evaluation continues with the next rule.
4. A **terminal rule** (`DocDir`, `Send-File`, `Abort`, `No-Content`, `Disallow`, the `MCP-Tool-*` rules) sends a response to the client and stops evaluation. Rules after it, in this block or any later one, do not run for that request.
5. If the end of the file is reached without a terminal rule, the response built so far is sent. Since it started as `404`, a path matched by no `DocDir` or `Send-File` gets a `404` with whatever headers were set along the way.

Because of step 4, order matters. Headers that should apply to every
response go first; specific routes go in the middle; the catch-all `DocDir`
goes last. A block placed after a catch-all is unreachable.

## Basic rules

These rules are always available; they do not need a module.

### `Set-Header NAME VALUE`

:   Set response header `NAME` to `VALUE`, replacing any earlier value. Quote
    values that contain spaces.

### `Set-Code CODE`

:   Set the status code of the response in progress. It is kept by the
    terminal rule that eventually sends the response (for example `Send-File`
    with `Set-Code 451` serves the file with status `451`).

### `Abort CODE` *(terminal)*

:   Stop and send an error response with status `CODE` and a short HTML body
    describing it.

### `No-Content` *(terminal)*

:   Stop and send `204 No Content` with the headers collected so far. Handy
    for connectivity probes such as `/generate_204`.

### `Disallow` *(terminal)*

:   Stop and send `403 Forbidden`.

## Context rules

Context rules come from modules. The ones shipped with the server:

| Rule | Module | Holds when |
|------|--------|------------|
| `Route PATTERN` | [route](../modules/route.md) | The request path matches the wildcard pattern. |
| `Host PATTERN` | [header](../modules/header.md) | The `Host` header matches the wildcard pattern. |
| `If-Header NAME` | [header](../modules/header.md) | The request carries header `NAME`. |
| `If-Header NAME VALUE` | [header](../modules/header.md) | Header `NAME` is present and equals `VALUE`. |
| `If-Env VAR VALUE` | [env](../modules/env.md) | Environment variable `VAR` equals `VALUE`. |
| `If-Not-Env VAR VALUE` | [env](../modules/env.md) | `VAR` is unset or differs from `VALUE`. |
| `If-Env-Def VAR` | [env](../modules/env.md) | `VAR` is set, whatever its value. |

A context rule with the wrong number of arguments never holds, so its block
is silently skipped.

### Wildcard patterns

`Route` and `Host` match with shell-style wildcards: `*` matches any sequence
of characters including `/`, and `?` matches exactly one character. The match
is anchored at both ends, so `Route /img` matches only `/img`, while
`Route /img*` also matches `/img/cat.png`. The request path is matched
without the query string.

## Rules from modules

| Rule | Module | Effect |
|------|--------|--------|
| `DocDir DIR` *(terminal)* | [static](../modules/static.md) | Serve the file under `DIR` named by the request path. |
| `Send-File PATH` *(terminal)* | [static](../modules/static.md) | Serve exactly `PATH`. |
| `MCP-Tool-Streamable "CMD" MS` *(terminal)* | [mcp](../modules/mcp.md) | Expose a stdio MCP tool over Streamable HTTP. |
| `MCP-Tool-SSE "CMD" MS` *(terminal)* | [mcp](../modules/mcp.md) | Expose it over the legacy SSE transport. |
| `MCP-Tool-SSE-Single "CMD" MS` *(terminal)* | [mcp](../modules/mcp.md) | SSE transport with one shared session per route. |

The complete list with defaults is on the [Directives](../reference/directives.md) page.

## `Include`

```text
Include "/etc/nya/mimes.conf"
```

Reads another file and evaluates its rules in place, as if they were written
where the `Include` stands. Included files may include further files. The
sample `mimes.conf` uses this to set `Content-Type` by file extension before
the catch-all `DocDir` runs.

## Applying a route several times

Since a terminal rule ends evaluation, split the work across blocks when one
path needs several decisions. This sets a header for everything, applies
MIME types, special-cases one host and finally serves from disk:

```text
# First set header which will be applied to all routes
Route *
  Set-Header X-Powered-By "Pusheen The Cat"
End

# Now apply mimes
Include "/etc/nya/mimes.conf"

# Now apply localhost rule, so if Host header is localhost,
# we will send localcat.html
Host localhost
  Set-Header Content-Type "text/html"
  Send-File "/var/www/html/localcat.html"
End

# Finally, apply default rule: send respective file from /var/www/html
Route *
  DocDir /var/www/html
End

# It is impossible to reach this route,
# since DocDir already would apply to it and
# send off request to the client.
Route /never_reachable
  Abort 418
End
```

## Request methods

Rules do not distinguish methods by themselves; a `Route` block runs for
`GET`, `HEAD`, `POST` and every other method alike. `POST` and other requests
with a body are additionally checked against `MaxPostSize` before any rule
runs (see [Server](server.md)). The `MCP-Tool-*` rules inspect the method
internally and answer `405` to methods they do not support.

See also the configurations in [Examples](examples.md) and the annotated test
configuration in
[`e2e/config/routes.conf`](https://github.com/0xf104a/MeowMeow/blob/master/e2e/config/routes.conf),
which exercises every rule.
