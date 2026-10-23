# `route` module

Provides the `Route` context rule, which matches the request path.

```text
LoadModules route,...
```

## `Route PATTERN`

Opens a block that applies when the request path matches `PATTERN`.

```text
Route /api/*
 Set-Header Cache-Control no-store
End

Route /img/???.png
 Set-Header Cache-Control "max-age=31536000"
End
```

- `*` matches any run of characters, including `/`. `?` matches exactly one character.
- The match is anchored: `Route /img` matches only `/img`; use `Route /img*` or `Route /img/*` for its contents.
- The query string is not part of the matched path. `/search?q=cats` is matched as `/search`.
- `Route *` matches every request and is the usual way to apply a rule globally.

Blocks nest with other context rules in any order:

```text
Host "127.0.0.1*"
 Route /duckduck*
  Set-Header X-Continuation goose
 End
End
```

`Route` only decides whether its block runs. The block must contain a
terminal rule such as `DocDir` or `Abort` to actually answer the request;
otherwise evaluation continues after `End`.
