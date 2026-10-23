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
LoadModules static,header,route,env
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

## Notes

- Values are taken verbatim after the directive name; `KeepAlive 10000 #ms` works because the comment is stripped first.
- There is no directive for TLS yet. Terminate TLS in a reverse proxy in front of MeowMeow.
