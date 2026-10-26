# Testing

MeowMeow is tested on two levels: EUnit suites for every Erlang module
(`test/`), and an end-to-end suite that drives a running server inside a
container from Ruby (`e2e/`). In CI the end-to-end job only runs after the
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

This builds `e2e/Dockerfile` as `test.meow.meow` and runs it. The image

1. installs Ruby and clones the [SimpleTest](https://github.com/0xf104a/SimpleTest) harness,
2. creates a one-gigabyte random file at `/var/www/FatCat` for the large-transfer test,
3. builds a production release from the working tree,
4. installs `e2e/config/*.conf` into `/etc/nya/` and `e2e/www/` into `/var/www/`,
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
| `e2e/simple_test/test_server.rb` | The `Server` header is `MeowMeow/<e2e/vsn.txt>.<commit>`; a one-gigabyte file arrives intact (MD5 compared) and is streamed rather than buffered. |
| `e2e/simple_test/test_rules.rb` | `Route`, nested `Host`/`Route`, `Set-Header`, `Set-Code` with `Send-File`, `Abort`, `No-Content`, `Include`, MIME types, `404` and `500` paths, quoted arguments. |
| `e2e/simple_test/test_env.rb` | `If-Env`, `If-Not-Env` and `If-Env-Def` against the `RUNTYPE` variable set by the test script. |
| `e2e/simple_mcp/server.rb` | A minimal stdio MCP server used to exercise the `mcp` module by hand. |

The routing file the tests run against is
[`e2e/config/routes.conf`](https://github.com/0xf104a/MeowMeow/blob/master/e2e/config/routes.conf);
`e2e/config/bad_meow.conf` and `nonsense.conf` exist to test error handling
and `Include`.

## Adding a test

For a unit test, add a `{"what is expected and why", ?_test(...)}` entry to the
matching `test/<module>_tests.erl` (or a new suite following the same
fixture pattern) and run `rebar3 eunit`.

For an end-to-end test:

1. Add the route to `e2e/config/routes.conf` (and files to `e2e/www/` if needed).
2. Add a `def self.test_<name>` method to the matching file in `e2e/simple_test/`. It must return `true` on success. `TestHTTP::get(url)` returns an object with `code`, `headers`, `data` and `duration`.
3. Run `./test.sh`.

## Version check

`e2e/vsn.txt` must equal the release version in `rebar.config`, because
the first test expects the `Server` header to be `MeowMeow/<vsn.txt>.<commit>`
(optionally followed by `.dirty`). Bump both together; see
[Releasing](releasing.md). The commit comes from the `GIT_SHA` build argument
of `e2e/Dockerfile`, which `test.sh` and CI pass; without it the header
reads `.unknown`, which the test also accepts.

## Continuous integration

| Workflow | Trigger | What it does |
|----------|---------|--------------|
| `CI` (`ci.yml`) | push to `master`, release tags `*.*-*`, pull requests (including the switch from draft to ready for review) | Job `eunit`: `rebar3 eunit` + `rebar3 cover` in the `erlang:28.4.1` image. Job `e2e` (`needs: eunit`): builds and runs the test image. Job `image` (`needs: eunit, e2e`, so the image is only built when the whole suite passed): builds the production image and pushes it to `ghcr.io` signed with cosign, tagged with the short commit hash, plus the version and `latest` on release tags. Fork and draft pull requests only build. |
| `Docs` (`docs.yml`) | push and PR to `master` | Stages the documentation and builds the site with `zensical build --strict`. |
| `Benchmark` (`benchmark.yml`) | release tags, manual | Load-tests the image and records the results in the documentation; see [Benchmarking](benchmarking.md). |

Pull requests run against the merge commit with `master`; the published image
is built from the head commit of the branch, which is the hash it is tagged
with.
