# Quickstart

This walkthrough gets a static site served in a few minutes and shows the
routing language along the way. It assumes you have completed
[Installation](installation.md).

## 1. Minimal server configuration

Create `/etc/nya/meow.conf`:

```text
ListenHost 127.0.0.1
ListenPort 8080
LogLevel 3
KeepAlive 10000
LoadModules static,header,route,env
```

- `ListenHost` must be an IP address; host names are not resolved.
- `LogLevel 3` prints info, warnings and errors. Use `4` while debugging.
- `LoadModules` activates the modules whose directives you want to use. Without `static` nothing is ever served.

The full list of directives is in [Server configuration](../configuration/server.md).

## 2. Minimal routing configuration

Create `/etc/nya/routes.conf`:

```text
# Content-Type based on the file extension
Include "/etc/nya/mimes.conf"

# A header applied to every response
Route *
 Set-Header X-Powered-By "Pusheen the Cat"
End

# Android/Chrome captive portal probe
Route /generate_204
 No-Content
End

# Everything else comes from disk
Route *
 DocDir /var/www/html
End
```

and copy `config/mimes.conf` from the repository to `/etc/nya/mimes.conf`.

The rules run top to bottom for every request. `Set-Header` only modifies the
response in progress, so evaluation continues. `No-Content` and `DocDir` are
*terminal*: they send a response and stop evaluation. That is why the
catch-all `DocDir` block comes last.

## 3. Something to serve

```bash
sudo mkdir -p /var/www/html
echo '<h1>Nya~ It works!</h1>' | sudo tee /var/www/html/index.html
```

When a request path points at a directory, MeowMeow looks for `index.html`
inside it.

## 4. Start and test

```bash
./_build/prod/rel/MeowMeow/bin/MeowMeow foreground
```

In another terminal:

```bash
curl -i http://127.0.0.1:8080/
```

```http
HTTP/1.1 200 OK
Content-Type: text/html
X-Powered-By: Pusheen the Cat
Server: MeowMeow/2.0-beta1.d98e0ab.
Content-Length: 24
...
```

```bash
curl -i http://127.0.0.1:8080/generate_204   # -> 204 No Content
curl -i http://127.0.0.1:8080/missing        # -> 404 Not Found
```

## 5. Add a condition

Rules can be nested under conditions. Serve a different page when the request
arrives for `localhost` but keep the header from step 2:

```text
Host localhost
 Set-Header Content-Type text/html
 Send-File /var/www/html/localcat.html
End
```

Put this block *before* the final `Route *` so the terminal `Send-File` wins.
`Host`, `Route`, `If-Header` and the `If-Env*` family are all *context rules*:
the block body only runs when the condition holds. See
[Routes](../configuration/routes.md) for the complete semantics.

## Next steps

- [Examples](../configuration/examples.md) collects larger real-world configurations.
- [mcp](../modules/mcp.md) shows how to expose an MCP tool over HTTP.
- [Docker](docker.md) explains how to run the same setup in a container.
