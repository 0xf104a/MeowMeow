# Directive reference

Every directive MeowMeow understands, on one page. *Terminal* rules send a
response and stop evaluation; *context* rules open a block closed by `End`.

## `meow.conf`

| Directive | Argument | Default | Meaning |
|-----------|----------|---------|---------|
| `ListenHost` | IPv4 address | `127.0.0.1` | Address to bind. |
| `ListenPort` | port | `80` | Port to listen on. |
| `LogLevel` | `0`–`4` | `0` | Log verbosity: nothing, errors, +warnings, +info, +debug. |
| `KeepAlive` | milliseconds | `10000` | Idle timeout of a keep-alive connection. |
| `AllowLegacyHttp` | `Yes`/`No` | `No` | Accept HTTP/1.0 requests instead of answering `505`. |
| `MaxPostSize` | bytes | `10240` | Largest accepted request body. |
| `LoadModules` | `a,b,c` | none | Modules to load, in order. |

Details: [Server configuration](../configuration/server.md).

## `routes.conf`: structure

| Directive | Meaning |
|-----------|---------|
| `Include "FILE"` | Evaluate the rules of `FILE` in place. |
| `End` | Close the innermost context block. |
| `# text` | Comment, to end of line. |

## `routes.conf`: basic rules (always available)

| Directive | Kind | Effect |
|-----------|------|--------|
| `Set-Header NAME VALUE` | rule | Set a response header. |
| `Set-Code CODE` | rule | Set the status code of the response in progress. |
| `Abort CODE` | terminal | Send the error page for `CODE`. |
| `No-Content` | terminal | Send `204 No Content`. |
| `Disallow` | terminal | Send `403 Forbidden`. |

## `routes.conf`: module rules

| Directive | Module | Kind | Effect |
|-----------|--------|------|--------|
| `Route PATTERN` | `route` | context | Request path matches the wildcard. |
| `Host PATTERN` | `header` | context | `Host` header matches the wildcard. |
| `If-Header NAME` | `header` | context | Header present. |
| `If-Header NAME VALUE` | `header` | context | Header equals `VALUE`. |
| `If-Env VAR VALUE` | `env` | context | Environment variable equals `VALUE`. |
| `If-Not-Env VAR VALUE` | `env` | context | Variable unset or different. |
| `If-Env-Def VAR` | `env` | context | Variable set. |
| `DocDir DIR` | `static` | terminal | Serve the request path from `DIR` (`index.html` for directories). |
| `Send-File PATH` | `static` | terminal | Serve `PATH`. |
| `MCP-Tool-Streamable "CMD" MS` | `mcp` | terminal | Streamable HTTP transport for the stdio MCP tool `CMD`; sessions idle out after `MS` ms. |
| `MCP-Tool-SSE "CMD" MS` | `mcp` | terminal | Legacy SSE transport. |
| `MCP-Tool-SSE-Single "CMD" MS` | `mcp` | terminal | SSE transport with one session per route. |

Wildcards: `*` any characters, `?` one character, anchored at both ends.

Details: [Routes](../configuration/routes.md) and the module pages under
[Modules](../modules/index.md).
