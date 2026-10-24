# Server configuration (`meow.conf`)

`meow.conf` is a flat list of `Directive Value` lines. Every directive has a
default, so an empty file is valid, but a useful server needs at least
`LoadModules`.

```text
# /etc/nya/meow.conf
ListenHost 0.0.0.0
ListenPort 80
LogLevel 3
KeepAlive 10000
AllowLegacyHttp No
MaxPostSize 1048576
MCPMaxSessions 8
MCPMemoryLimit 512M
LoadModules static,header,route,env,mcp
```

## Directives

### `ListenHost`

:   IP address to bind. Must be a literal IPv4 address; host names are not
    resolved. Use `0.0.0.0` to accept connections on every interface, which is
    what you want inside a container.

    **Default:** `127.0.0.1`

### `ListenPort`

:   TCP port to listen on. Ports below 1024 need the usual privileges.

    **Default:** `80`

### `LogLevel`

:   Verbosity of the log written to standard output.

    | Level | Output |
    |-------|--------|
    | `0` | Nothing. |
    | `1` | Errors. |
    | `2` | Errors and warnings. |
    | `3` | Errors, warnings and info, including one line per handled request. |
    | `4` | Everything, including debug output for each rule evaluation. |

    **Default:** `0`

### `KeepAlive`

:   How long, in milliseconds, an idle keep-alive connection is kept open
    before the server closes it. The timer restarts after every request on
    the connection. MeowMeow answers HTTP/1.1 requests with
    `Connection: keep-alive` unless the client asked for `Connection: close`.

    **Default:** `10000`

### `AllowLegacyHttp`

:   `Yes` or `No`. When `No`, requests with a protocol version below HTTP/1.1
    (that is, HTTP/1.0 and HTTP/0.9) are rejected with
    `505 HTTP Version Not Supported`. Set to `Yes` only if you must serve very
    old clients.

    **Default:** `No`

### `MaxPostSize`

:   Maximum accepted `Content-Length` of a request body, in bytes. Larger
    bodies are rejected with `413 Payload Too Large` before any rule runs. A
    body without `Content-Length` is rejected with `411 Length Required`.
    Raise this when exposing MCP tools that receive large arguments.

    **Default:** `10240`

### `LoadModules`

:   Comma-separated list of modules to load at startup, in order. A module's
    directives are only recognised in `routes.conf` after it has been loaded;
    an unknown directive aborts startup. See [Modules](../modules/index.md)
    for the list of modules that ship with the server.

    **Default:** none. Without `static` no file is ever served.

    ```text
    LoadModules static,header,route,env,mcp
    ```

### `MCPMaxSessions`

:   Largest number of MCP tool sessions (subprocesses) alive at the same time,
    over every `MCP-Tool-*` rule. A request that needs a new session while the
    cap is reached waits up to `MCPSessionWaitMs` for a slot and is otherwise
    answered `503 Service Unavailable` with `Retry-After`. `0` means no cap.
    Per-command caps are set on the rule with `MaxSessions=N`; see
    [mcp](../modules/mcp.md#session-caps).

    **Default:** `0`

### `MCPSessionWaitMs`

:   How long, in milliseconds, a request may wait for a free session slot
    before it gets `503`. `0` refuses at once.

    **Default:** `5000`

### `MCPMemoryLimit`

:   Default memory limit of one MCP session's process tree, as bytes or with
    a `K`, `M` or `G` suffix (`512M`). `0` means no limit. A rule's
    `MemoryLimit=SIZE` overrides it. See
    [mcp](../modules/mcp.md#memory-limits) for how it is enforced.

    **Default:** `0`

### `MCPMemoryPollMs`

:   Period, in milliseconds, at which each session's memory is sampled.
    With the `/proc` backend this is also how often the limit is checked.
    `0` disables sampling (and software enforcement).

    **Default:** `1000`

### `MCPCgroup`

:   `Yes` or `No`. When `Yes`, the server tries to put every MCP session in
    its own cgroup v2 so the kernel meters and enforces its memory limit,
    and falls back to `/proc` polling when the hierarchy is not writable.
    `No` skips the attempt.

    **Default:** `Yes`

### `MCPCgroupRoot`

:   Directory of the cgroup to create session cgroups under. Empty means the
    cgroup the server runs in, read from `/proc/self/cgroup`. Only needed for
    unusual setups.

    **Default:** empty

## Notes

- Values are taken verbatim after the directive name; `KeepAlive 10000 #ms` works because the comment is stripped first.
- There is no directive for TLS yet. Terminate TLS in a reverse proxy in front of MeowMeow.
