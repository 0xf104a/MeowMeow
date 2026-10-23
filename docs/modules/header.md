# `header` module

Provides two context rules that look at request headers.

```text
LoadModules header,...
```

## `Host PATTERN`

Opens a block that applies when the request's `Host` header matches the
wildcard `PATTERN` (same `*` and `?` semantics as `Route`). Requests without
a `Host` header are matched against the empty string.

```text
Host "docs.example.org"
 DocDir /srv/www/docs
End

Host "*.example.org"
 DocDir /srv/www/main
End
```

The header is matched verbatim, so a client connecting to a non-default port
sends `Host: example.org:8080`. Use `"example.org*"` or list the port to
match it.

## `If-Header NAME`

Applies when the request carries header `NAME`, whatever its value.

```text
If-Header X-Forwarded-For
 Set-Header X-Behind-Proxy yes
End
```

## `If-Header NAME VALUE`

Applies when header `NAME` is present and its value is exactly `VALUE`.
Quote values with spaces.

```text
Route /debug*
 If-Header X-Debug-Token "s3cret"
  Send-File /var/www/debug.html
 End
 Disallow
End
```

The comparison is case sensitive on the value. There is no negated form; put
the fallback rule after the block, as above.
