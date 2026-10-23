# Status codes

Which HTTP status MeowMeow sends on its own, and why. Statuses chosen in
`routes.conf` with `Abort` or `Set-Code` are not listed.

| Status | Sent when |
|--------|-----------|
| `200 OK` | `DocDir` or `Send-File` served a file (unless overridden with `Set-Code`); an MCP request got its response. |
| `201 Created` | `MCP-Tool-Streamable`: a session was created by an `initialize` request. |
| `202 Accepted` | `mcp`: a notification was forwarded, or a body was accepted on the SSE transport. |
| `204 No Content` | `No-Content`; `mcp`: a session was deleted. |
| `400 Bad Request` | `mcp`: a request needed a session id but carried none. |
| `403 Forbidden` | `Disallow`; `DocDir` could not read the file. |
| `404 Not Found` | No terminal rule produced a response; `DocDir` found no file; `mcp`: unknown or expired session. |
| `405 Method Not Allowed` | `mcp`: a method other than the transport's `GET`/`POST`/`DELETE`. |
| `406 Not Acceptable` | `MCP-Tool-Streamable`: `GET` without `Accept` covering both `application/json` and `text/event-stream`. |
| `411 Length Required` | A request with a body (`POST`) had no `Content-Length`. |
| `413 Payload Too Large` | `Content-Length` exceeds `MaxPostSize`. |
| `415 Unsupported Media Type` | `mcp`: `POST` whose `Content-Type` is not `application/json`. |
| `422 Unprocessable Content` | `MCP-Tool-Streamable`: `initialize` request without a JSON-RPC `id`. |
| `500 Internal Server Error` | A directive in the rule table has no handler (should not happen after a successful start); `Send-File` could not stat its file. |
| `501 Not Implemented` | `MCP-Tool-Streamable`: `Mcp-Protocol-Version` missing or older than `2025-11-25`. |
| `502 Bad Gateway` | A rule crashed or returned an unexpected value; the error and stack trace are logged. |
| `504 Gateway Timeout` | `mcp`: the tool's subprocess ended while a response was awaited. |
| `505 HTTP Version Not Supported` | HTTP/1.0 request while `AllowLegacyHttp` is `No`. |

Error responses carry a short HTML body naming the status, the `Server`
header and `Date`.
