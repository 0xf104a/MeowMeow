# `env` module

Provides context rules that read the server process's environment
variables. They make a single, static `routes.conf` behave differently per
deployment, which is especially useful in containers.

```text
LoadModules env,...
```

## `If-Env VAR VALUE`

Applies when environment variable `VAR` is set and equals `VALUE`.

```text
If-Env RUNTYPE docker
 Set-Header X-Environment docker
End
```

## `If-Not-Env VAR VALUE`

Applies when `VAR` is unset **or** differs from `VALUE`.

```text
If-Not-Env RUNTYPE docker
 Abort 500
End
```

## `If-Env-Def VAR`

Applies when `VAR` is set, whatever its value, including the empty string.

```text
If-Env-Def RUNTYPE
 Set-Header X-Environment-Present 1
End
```

## Notes

- Variables are read at request time from the Erlang VM's environment, so they must be set before the server starts (`MAINTENANCE=on MeowMeow foreground`, or `-e` with `docker run`).
- A rule with the wrong number of arguments never applies.
- Values are compared exactly; there is no wildcard matching on values.
