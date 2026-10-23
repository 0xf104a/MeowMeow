# Testing

MeowMeow is tested on two levels: EUnit suites for every Erlang module
(`test/`), and an end-to-end suite that drives a running server inside a
container from Ruby (`tests/`). In CI the end-to-end job only runs after the
unit tests have passed, and `./test.sh` mirrors that order locally.

## Unit tests (EUnit)

Each module has a suite `test/<module>_tests.erl`; shared fixtures live in
`test/test_support.erl`. The suites need no configuration files, no root and
no network beyond the loopback interface, so they run anywhere `rebar3` does:

```bash
rebar3 eunit                               # every suite
rebar3 eunit --module=parse_http_tests     # one suite
rebar3 cover --verbose                     # coverage summary after a run
```

Without a local Erlang (for example inside a Flatpak IDE) run them in the
same image CI uses:

```bash
docker run --rm -v "$PWD":/src:ro -w /build erlang:28.4.1 \
  sh -c 'cp -r /src/. . && rebar3 eunit'
```

How the suites are built:

- The test profile in `rebar.config` compiles with `export_all`, so suites can
  call internal helpers (config parsers, rule handlers, `handle:handle/2`)
  directly. Release builds are unaffected.
- Global state (`config_storage`, `rules`, `rules_context`, `access`,
  `mcp_sessions`) is created inside EUnit `setup` fixtures by
  `test_support`, as public ETS tables, and dropped afterwards.
- Requests are built by pushing raw HTTP bytes through
  `parse_http:update_request/2`, exactly like the socket handler does, so tests
  see the real `#request{}` shape (binary method/route, lower-cased header
  names).
- Code that talks to the TCP control process is run with the test process as
  the fake upstream and the messages it sends (`{send, Data}`, `close`,
  `set_tmr`, ...) are asserted. Code that needs a real socket
  (`handle:handler_start/1`, `static_handler`, `nya_tcp`, `server:listen/1`)
  gets a loopback socket pair.
- The MCP suites use `cat` as the stdio tool: every JSON-RPC line written to it
  comes back, which makes request/response pairing observable.
- `server:abort_init/1` halts the VM, so the `access` error-path tests swap the
  `server` module for a stub that records the call.

Every test title states the expected behaviour and where it comes from
(README, RFC, MCP specification or the function's own documentation). Titles
carrying a `(regression: this used to fail because ...)` note describe the
defect the test was written against. When a test goes red, fix the code, not
the test, unless the reasoning in the title is wrong.

## End-to-end tests

Docker is the only requirement:

```bash
./test.sh               # EUnit in Docker first, then the E2E image
./test.sh --skip-eunit  # E2E only
```

This builds `tests/Dockerfile` as `test.meow.meow` and runs it. The image

1. installs Ruby and clones the [SimpleTest](https://github.com/0xf104a/SimpleTest) harness,
2. creates a one-gigabyte random file at `/var/www/FatCat` for the large-transfer test,
3. builds a production release from the working tree,
4. installs `tests/config/*.conf` into `/etc/nya/` and `tests/www/` into `/var/www/`,
5. on start, launches `MeowMeow daemon` with `TESTROOT=/tests` and `RUNTYPE=docker` in the environment and runs the Ruby tests against `http://localhost`.

The container's exit status is the test result, which is what the `Tests`
GitHub workflow checks on every push.

To poke at the server before the tests run, start the image with a shell:

```bash
docker run -it test.meow.meow shell
```

## What is covered

| File | Covers |
|------|--------|
| `tests/simple_test/test_server.rb` | The `Server` header matches `tests/vsn.txt`; a one-gigabyte file arrives intact (MD5 compared) and is streamed rather than buffered. |
| `tests/simple_test/test_rules.rb` | `Route`, nested `Host`/`Route`, `Set-Header`, `Set-Code` with `Send-File`, `Abort`, `No-Content`, `Include`, MIME types, `404` and `500` paths, quoted arguments. |
| `tests/simple_test/test_env.rb` | `If-Env`, `If-Not-Env` and `If-Env-Def` against the `RUNTYPE` variable set by the test script. |
| `tests/simple_mcp/server.rb` | A minimal stdio MCP server used to exercise the `mcp` module by hand. |

The routing file the tests run against is
[`tests/config/routes.conf`](https://github.com/0xf104a/MeowMeow/blob/master/tests/config/routes.conf);
`tests/config/bad_meow.conf` and `nonsense.conf` exist to test error handling
and `Include`.

## Adding a test

For a unit test, add a `{"what is expected and why", ?_test(...)}` entry to the
matching `test/<module>_tests.erl` (or a new suite following the same
fixture pattern) and run `rebar3 eunit`.

For an end-to-end test:

1. Add the route to `tests/config/routes.conf` (and files to `tests/www/` if needed).
2. Add a `def self.test_<name>` method to the matching file in `tests/simple_test/`. It must return `true` on success. `TestHTTP::get(url)` returns an object with `code`, `headers`, `data` and `duration`.
3. Run `./test.sh`.

## Version check

`tests/vsn.txt` must equal the release version in `rebar.config`, because
the first test compares the `Server` header with it. Bump both together; see
[Releasing](releasing.md).

## Continuous integration

| Workflow | Trigger | What it does |
|----------|---------|--------------|
| `Erlang CI` (`erlang.yml`) | push and PR to `master` | `rebar3 compile` in the `erlang:28.4.1` image. |
| `Tests` (`test.yml`) | every push | Job `eunit`: `rebar3 eunit` + `rebar3 cover` in the `erlang:28.4.1` image. Job `e2e` (`needs: eunit`, so it only runs when the unit tests pass): builds and runs the test image. |
| `Docker` (`docker-publish.yml`) | version tags, PRs | Builds the production image; on tags pushes it to `ghcr.io` and signs it with cosign. |
| `Docs` (`docs.yml`) | push and PR to `master` | Stages the documentation and builds the site with `zensical build --strict`. |
