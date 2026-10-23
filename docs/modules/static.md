# `static` module

Serves files from disk. Both of its directives are terminal: they send the
response and stop evaluation.

```text
LoadModules static,...
```

## `DocDir DIRECTORY`

Serves the file under `DIRECTORY` whose relative path is the request path.
With

```text
Route *
 DocDir /var/www/html
End
```

a request for `/css/site.css` sends `/var/www/html/css/site.css`.

- When the path names a directory, `index.html` inside it is served.
- A missing file answers `404 Not Found`.
- A file that cannot be read answers `403 Forbidden`.
- The response carries `Content-Length`, `Date` and `Server`. `Content-Type` is **not** inferred from the file; set it with `Set-Header`, typically by including `mimes.conf` before the `DocDir` block.
- Large files are streamed to the socket in 64 KiB chunks, so memory use does not grow with file size. The test suite checks a one-gigabyte transfer.

The directory argument may be quoted: `DocDir "/var/www/my site"`.

## `Send-File PATH`

Sends exactly `PATH`, regardless of the request path.

```text
Route /catnip
 Set-Code 451
 Send-File /var/www/catnip.html
End
```

- The status code is `200` unless `Set-Code` was applied earlier in the evaluation, as above.
- Headers set earlier (`Set-Header`) are kept, so include `Content-Type` yourself.
- If the file cannot be stat'ed, the request is answered with `500` and the reason is logged.

## Notes

- Neither directive looks at the request method; a `POST` to a static route gets the file too.
- Paths are used as given. Put a `Route /.*` with `Disallow` before your `DocDir` block if dot-files must not be served.
