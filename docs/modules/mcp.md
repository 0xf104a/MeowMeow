# `mcp` module

Turns a [Model Context Protocol](https://modelcontextprotocol.io/) server that
speaks JSON-RPC over **stdio** into one reachable over **HTTP**. MeowMeow
starts the tool as a subprocess per session, forwards request bodies to its
standard input and relays the lines it prints back to the client over the
transport the client chose.

```text
LoadModules static,header,route,env,mcp
```

Three transports are available, each as a terminal rule that takes the same
arguments:

```text
Rule "COMMAND" KEEPALIVE_MS [MaxSessions=N] [MemoryLimit=SIZE]
```

| Argument | Meaning |
|----------|---------|
| `COMMAND` | Command line that starts the tool. Quote it when it has spaces. The executable is looked up on the server's `PATH`. |
| `KEEPALIVE_MS` | Idle time in milliseconds after which a session's subprocess is terminated. |
| `MaxSessions=N` | Optional. At most `N` live sessions started with this command, across every route that uses it. `0` disables the rule. See [Session caps](#session-caps). |
| `MemoryLimit=SIZE` | Optional. Memory the session's process tree may use, e.g. `256M`, `2G` or a number of bytes; `0` means no limit. Overrides `MCPMemoryLimit` from `meow.conf`. See [Memory limits](#memory-limits). |

Option names are not case-sensitive. A rule with a non-numeric keep-alive,
an unknown option or a bad option value answers every request with `500`
and logs the problem.

## `MCP-Tool-Streamable`

The current [Streamable HTTP transport](https://modelcontextprotocol.io/specification/2025-11-25/basic/transports)
(protocol revision `2025-11-25`).

```text
Route /mcp
 MCP-Tool-Streamable "/opt/tools/bin/weather-mcp" 60000
End
```

Request handling on that route:

| Request | Result |
|---------|--------|
| `POST` with an `initialize` JSON-RPC request | Starts a new subprocess and session. The tool's `initialize` result is returned as JSON with status `201` and the new id in the `Mcp-Session-Id` response header. |
| `POST` with `Mcp-Session-Id` and a JSON-RPC **request** (has `id`) | Forwards it to the tool and returns the matching response as `application/json`. |
| `POST` with `Mcp-Session-Id` and a JSON-RPC **notification** (no `id`) | Forwards it and returns `202 Accepted` with an empty body. |
| `GET` with `Mcp-Session-Id` | Opens a `text/event-stream` for server-to-client messages. The client must accept both `application/json` and `text/event-stream`. |
| `DELETE` with `Mcp-Session-Id` | Terminates the session's subprocess and returns `204`. |

Requirements and the status codes returned when they are not met:

| Condition | Status |
|-----------|--------|
| `Mcp-Protocol-Version` header older than `2025-11-25` | `400` |
| `initialize` while no session slot is free (see [Session caps](#session-caps)) | `503` with `Retry-After` |
| `POST` without a session that is not an `initialize` request | `400` |
| `POST` whose `Content-Type` is not `application/json` | `415` |
| `initialize` request without an `id` | `422` |
| Unknown or expired `Mcp-Session-Id` | `404` |
| `GET` or `DELETE` without a session | `400` |
| `GET` without the right `Accept` | `406` |
| Subprocess exits while a response is awaited | `504` |
| Any other method | `405` |

!!! note "Protocol version header"
    A request without `Mcp-Protocol-Version` is accepted, since the header
    is only mandatory after initialization. A header naming a revision older
    than `2025-11-25` is refused with `400`, as both the `2025-11-25` and the
    `2026-07-28` revisions require. Newer revisions are not refused.

!!! info "Clients speaking revision 2026-07-28"
    Revision [`2026-07-28`](https://modelcontextprotocol.io/specification/2026-07-28/changelog)
    made MCP stateless: no `initialize`, no `Mcp-Session-Id`, the protocol
    version travels in `_meta` on every request. MeowMeow is a *legacy*
    server in those terms. A dual-era client probes with a session-less
    request, gets the plain `400` error page, which is not a JSON-RPC error,
    and falls back to the `initialize` handshake as the
    [backward compatibility rules](https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http#backward-compatibility)
    tell it to. From then on it is served exactly like a `2025-11-25`
    client; the `Mcp-Method` and `Mcp-Name` headers and the `_meta` keys are
    passed through untouched. Clients that only speak `2026-07-28` cannot use
    this transport yet.

## `MCP-Tool-SSE`

The legacy [HTTP with SSE transport](https://modelcontextprotocol.io/specification/2024-11-05/basic/transports)
(protocol revision `2024-11-05`), for clients that have not moved to
Streamable HTTP.

```text
Route /sse
 MCP-Tool-SSE "/opt/tools/bin/weather-mcp" 60000
End
```

| Request | Result |
|---------|--------|
| `GET /sse` | Starts a subprocess and session and opens a `text/event-stream`. The first event is `endpoint` with data `/sse?sessionId=<id>`; every line the tool prints afterwards is pushed as a `message` event. |
| `GET /sse?sessionId=<id>` | Reconnects to an existing session's stream. |
| `POST /sse?sessionId=<id>` with JSON | Forwards the body to the tool and returns `202`. The tool's answer arrives on the stream. |
| `DELETE /sse?sessionId=<id>` | Terminates the session, `204`. |

`POST` without `sessionId` is `400`, a non-JSON `Content-Type` is `415`, an
unknown session is `404`, other methods are `405`. `GET /sse` while no
session slot is free is `503` with `Retry-After` instead of a stream.

## `MCP-Tool-SSE-Single`

A variant of the SSE transport with **one shared session per route** instead
of one per client. It exists for clients that cannot carry the `sessionId`
from the `endpoint` event into their `POST` URL.

```text
Route /sse-single
 MCP-Tool-SSE-Single "/opt/tools/bin/weather-mcp" 60000
End
```

- `GET` opens the stream, creating the route's session if none exists.
- `POST` forwards the body to the route's session, creating it if needed, and returns `202`.
- `DELETE` terminates the route's session.
- A `GET` or `POST` that would have to create the session while no session slot is free answers `503` with `Retry-After`.

Every client of the route talks to the same subprocess and sees the same
stream, so use it only behind a single trusted client.

## Sessions and lifetime

- Each session owns one subprocess, started with the configured command. Its standard output is read line by line; every complete line is a JSON-RPC message delivered to the connected client(s).
- A session is terminated when the client deletes it, when the subprocess exits, or after `KEEPALIVE_MS` of inactivity. Terminated sessions answer `404` afterwards and the client must initialise again.
- Session ids are 64 random bytes, so they are safe to use as the only credential *of a session*, but they do not protect the endpoint itself.

## Session caps

Every session is a subprocess, and nothing stops a busy or hostile client
from starting more of them than the host can hold. Two caps bound that:

| Setting | Scope | Meaning |
|---------|-------|---------|
| `MCPMaxSessions N` in `meow.conf` | whole server | At most `N` live sessions over all tools and routes. `0` (the default) means no cap. |
| `MaxSessions=N` on a rule | one command | At most `N` live sessions started with that command. |

A session counts from the moment its subprocess starts until it ends for any
reason: `DELETE`, keep-alive expiry, the tool exiting, or a memory limit.

When a request needs a new session and no slot is free it **waits** up to
`MCPSessionWaitMs` (default `5000`) for one to be released, holding its
connection open. At most as many requests wait as the cap allows to run;
beyond that, and when the wait runs out, the request is answered with
`503 Service Unavailable` and a `Retry-After` header (the wait time in
seconds, at least `1`). Set `MCPSessionWaitMs 0` to refuse immediately.

```text
# meow.conf
MCPMaxSessions 8
MCPSessionWaitMs 3000
```

```text
# routes.conf
Route /mcp
 MCP-Tool-Streamable "npx -y @modelcontextprotocol/server-filesystem /data" 60000 MaxSessions=2
End
```

Caps are checked by one process, so they hold under concurrent requests.
Waiting requests are served oldest first; one whose command is still at its
own `MaxSessions` is skipped in favour of a younger one that fits.

## Memory limits

Each session's whole process tree is metered, not only the process the
server started: a tool launched as `npx ...` is a shell, a Node process
and whatever that forks. `MemoryLimit=SIZE` on the rule, or
`MCPMemoryLimit` in `meow.conf` as the default for every rule, caps that
tree. A session over its limit is terminated and connected clients see the
stream end (`504` on a pending Streamable request); the log says which tool
and how much.

How the tree is metered and the limit enforced depends on what the host
offers:

| Backend | When | Enforcement |
|---------|------|-------------|
| **cgroup** | cgroup v2 with the `memory` controller is writable at the server's own cgroup. In a container running as root this is the normal case (Docker with its default private cgroup namespace, Podman with crun). | The kernel enforces the limit (`memory.max`). Every session gets its own cgroup, joined before the tool is started, so even daemonised descendants are covered and `cgroup.kill` ends them all. |
| **proc** | Anything else, or `MCPCgroup No`. | The server sums the resident set sizes of the tree it finds under the tool's pid in `/proc` every `MCPMemoryPollMs` (default `1000`) and kills the tree when a sample is over the limit. Short bursts between samples and processes that re-parent to init escape it, and shared pages are counted once per process, so give the limit some headroom. |

The backend is chosen once at startup and logged at `LogLevel 3`.
`MCPCgroupRoot` points the cgroup backend at a directory other than the one
read from `/proc/self/cgroup`, for unusual setups.

```text
# meow.conf
MCPMemoryLimit 512M      # default for every MCP-Tool-* rule
MCPMemoryPollMs 1000
MCPCgroup Yes
```

```text
# routes.conf
Route /mcp
 MCP-Tool-Streamable "/opt/tools/bin/weather-mcp" 60000 MemoryLimit=128M
End
```

!!! note "cgroup setup"
    To enable the memory controller for per-session cgroups, cgroup v2
    requires the server's own cgroup to have no member processes. On startup
    the server therefore moves the processes it finds there (itself) into a
    `meowmeow-main` child cgroup and creates sessions under `meowmeow-mcp`.
    Both live inside the cgroup the server was started in, so container
    limits set from outside still apply to everything.

Whatever the backend, terminating a session now kills the tool's whole
process tree instead of only its first process.

## Securing the endpoint

MeowMeow has no built-in authentication. Gate the rule with a context rule
and fall back to an error:

```text
Route /mcp
 If-Header Authorization "Bearer s3cret"
  MCP-Tool-Streamable "/opt/tools/bin/weather-mcp" 60000
 End
 Abort 401
End
```

and terminate TLS in a reverse proxy. Keep in mind that whoever can edit
`routes.conf` chooses the command the server executes.

## Limits that matter

- Request bodies go through the normal `POST` checks: `Content-Length` is required (`411`) and must not exceed `MaxPostSize` (`413`, default 10 KiB). Raise `MaxPostSize` in `meow.conf` for tools with large arguments.
- One subprocess per session: a busy public endpoint can start many processes. Pick `KEEPALIVE_MS` accordingly and set [session caps](#session-caps) and [memory limits](#memory-limits).
- Tool output must be newline-delimited JSON-RPC, one message per line, which is what stdio MCP servers produce.

## Trying it out

`tests/simple_mcp/server.rb` in the repository is a minimal stdio MCP server
in Ruby that answers `initialize`, lists one tool and executes it. With
`ruby` installed:

```text
Route /mcp
 MCP-Tool-Streamable "ruby /path/to/MeowMeow/tests/simple_mcp/server.rb" 30000
End
```

```bash
curl -i http://127.0.0.1:8080/mcp \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json, text/event-stream' \
  -H 'Mcp-Protocol-Version: 2025-11-25' \
  -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-11-25","capabilities":{},"clientInfo":{"name":"curl","version":"0"}}}'
```

The `Mcp-Session-Id` header of the `201` response is what subsequent
`tools/list` and `tools/call` requests must send back.
