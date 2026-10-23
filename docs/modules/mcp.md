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
two arguments:

```text
Rule "COMMAND" KEEPALIVE_MS
```

| Argument | Meaning |
|----------|---------|
| `COMMAND` | Command line that starts the tool. Quote it when it has spaces. The executable is looked up on the server's `PATH`. |
| `KEEPALIVE_MS` | Idle time in milliseconds after which a session's subprocess is terminated. |

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
| `Mcp-Protocol-Version` header missing or older than `2025-11-25` | `501` |
| `POST` without a session that is not an `initialize` request | `400` |
| `POST` whose `Content-Type` is not `application/json` | `415` |
| `initialize` request without an `id` | `422` |
| Unknown or expired `Mcp-Session-Id` | `404` |
| `GET` without a session | `400`; without the right `Accept` | `406` |
| Subprocess exits while a response is awaited | `504` |
| Any other method | `405` |

!!! note "Protocol version header"
    The server treats a missing `Mcp-Protocol-Version` header as the older
    `2025-03-26` revision and answers `501`. Clients must send
    `Mcp-Protocol-Version: 2025-11-25` on every request.

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
unknown session is `404`, other methods are `405`.

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

Every client of the route talks to the same subprocess and sees the same
stream, so use it only behind a single trusted client.

## Sessions and lifetime

- Each session owns one subprocess, started with the configured command. Its standard output is read line by line; every complete line is a JSON-RPC message delivered to the connected client(s).
- A session is terminated when the client deletes it, when the subprocess exits, or after `KEEPALIVE_MS` of inactivity. Terminated sessions answer `404` afterwards and the client must initialise again.
- Session ids are 64 random bytes, so they are safe to use as the only credential *of a session*, but they do not protect the endpoint itself.

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
- One subprocess per session: a busy public endpoint can start many processes. Pick `KEEPALIVE_MS` accordingly.
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
