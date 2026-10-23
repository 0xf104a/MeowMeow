# Configuration examples

Each example is a complete `routes.conf` unless stated otherwise, and assumes
`LoadModules static,header,route,env` in `meow.conf` (plus `mcp` where MCP
rules are used).

## Static site with MIME types

The configuration shipped in `config/`:

```text
Include "/etc/nya/mimes.conf"

Route *
 Set-Header X-Powered-By "Pusheen the Cat"
 DocDir "/var/www/html"
End

Route /generate_204
 No-Content
End
```

`mimes.conf` sets `Content-Type: application/octet-stream` for everything and
then overrides it per extension (`/*.html`, `/*.css`, `/*.png`, ...). Because
`DocDir` is terminal, `/generate_204` is only reached when
`/var/www/html/generate_204` does not exist and the request fell through
with a `404`. To make the probe reliable, move its block above the `DocDir`
one.

## Several virtual hosts

```text
Include "/etc/nya/mimes.conf"

Host "docs.example.org"
 DocDir /srv/www/docs
End

Host "*.example.org"
 DocDir /srv/www/main
End

# Anything else: refuse
Disallow
```

`Host` patterns are wildcards matched against the raw `Host` header, so
include the port (`"example.org:8080"` or `"example.org*"`) when clients
connect to a non-default port.

## Maintenance page toggled by the environment

```text
If-Env MAINTENANCE on
 Set-Code 503
 Set-Header Retry-After 600
 Set-Header Content-Type text/html
 Send-File /srv/www/maintenance.html
End

Include "/etc/nya/mimes.conf"

Route *
 DocDir /srv/www/site
End
```

Start the server with `MAINTENANCE=on` to serve the maintenance page with
status `503` for every path. `Set-Code` applies to the file sent by
`Send-File`.

## Blocking paths and clients

```text
# Never expose dot-files or version control metadata
Route /.*
 Disallow
End

# A legal takedown, with the right status
Route /catnip
 Set-Code 451
 Send-File /var/www/catnip.html
End

# A debug endpoint only for requests that carry a shared secret
Route /debug*
 If-Header X-Debug-Token "s3cret"
  Send-File /var/www/debug.html
 End
 Disallow
End

Include "/etc/nya/mimes.conf"

Route *
 DocDir /var/www
End
```

The `/debug*` block shows the pattern for "allow if, otherwise deny": the
inner `If-Header` block ends with a terminal rule, so only requests without
the right header reach `Disallow`.

## MCP gateway next to a static site

```text
# JSON-RPC tool over Streamable HTTP, sessions idle out after 60 s
Route /mcp
 If-Header Authorization "Bearer s3cret"
  MCP-Tool-Streamable "/opt/tools/bin/weather-mcp" 60000
 End
 Abort 401
End

# Same tool over the legacy SSE transport for older clients
Route /sse
 MCP-Tool-SSE "/opt/tools/bin/weather-mcp" 60000
End

Include "/etc/nya/mimes.conf"

Route *
 DocDir /var/www/html
End
```

Remember to raise `MaxPostSize` in `meow.conf` if tool calls carry large
arguments; the default is 10 KiB. See [mcp](../modules/mcp.md) for the
transport details.

## Test configuration

The integration suite runs against
[`tests/config/routes.conf`](https://github.com/0xf104a/MeowMeow/blob/master/tests/config/routes.conf),
which combines nested `Host` and `Route` blocks, all three `If-Env*` rules,
`Set-Code` with `Send-File`, an `Include` of a second file and a one-gigabyte
file transfer. It is a good reference for the exact syntax every rule
accepts.
