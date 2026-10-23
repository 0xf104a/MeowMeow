# Configuration overview

MeowMeow is configured by two plain-text files. Their locations are compiled
into the binary (`src/config.hrl`); the defaults are:

| File | Default path | Documented in |
|------|--------------|---------------|
| `meow.conf` | `/etc/nya/meow.conf` | [Server](server.md) |
| `routes.conf` | `/etc/nya/routes.conf` | [Routes](routes.md) |

`meow.conf` describes *how the server runs*: where it listens, how much it
logs, which limits apply and which modules to load. `routes.conf` describes
*what happens to each request*. Both are read once at startup.

## Shared syntax

Both files are line oriented. Every line is a directive followed by its
arguments, separated by spaces:

```text
Directive Arg1 Arg2 ...
```

- A `#` starts a comment that runs to the end of the line. Comments may follow a directive on the same line.
- Blank lines and leading or trailing whitespace are ignored.
- Directive names are case sensitive: `ListenPort` works, `listenport` does not.

### Arguments and quoting

In `routes.conf` arguments are split on spaces, and an argument that contains
spaces must be quoted with double or single quotes:

```text
Set-Header X-Powered-By "Pusheen the Cat"
Include '/etc/nya/mimes.conf'
```

Quotes around an argument without spaces are allowed and removed.

In `meow.conf` a directive has exactly one value: everything after the first
space, with surrounding whitespace trimmed. Quotes are *not* interpreted
there.

### Blocks

Only `routes.conf` has structure. A *context rule* opens a block that runs to
the matching `End`, and blocks nest:

```text
Host "127.0.0.1*"
 Set-Header X-Different-Host true
 Route /duckduck*
  Set-Header X-Continuation goose
 End
End
```

Indentation is conventional, not required. `Include "path"` splices another
file in place, and the included file may itself contain blocks and includes.

## Errors

- A **missing `meow.conf`** or `routes.conf` aborts startup.
- An **unknown directive** in `routes.conf` (a name that no loaded module and no basic rule provides) aborts startup with exit status `255` and an error naming the directive. The usual cause is a module missing from `LoadModules`.
- An **`Include` of a missing file** is logged and makes the routing table fail to load, so the server refuses to start.
- **Structural mistakes** such as a missing `End` or a wrong number of arguments are *not* detected at load time. A rule called with unexpected arguments fails at request time; the request is answered with `502` and the error is logged. Keep `LogLevel 4` on while editing routes.
- **Unknown directives in `meow.conf`** are stored and ignored. Typos there silently fall back to the default value, so double check names like `ListenHost`.

## Reloading

There is no signal handler for reloading yet. Restart the server after
changing either file. In an attached console (`MeowMeow console` or
`rebar3 shell`) the routing table can be reloaded by hand with
`access:reload().`, which re-reads `routes.conf` without restarting.
