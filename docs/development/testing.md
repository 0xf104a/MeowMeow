# Testing

MeowMeow has no unit tests in Erlang; it is tested end to end against a
running server inside a container, with the HTTP client driven from Ruby.

## Running the suite

Docker is the only requirement:

```bash
./test.sh
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
| `Tests` (`test.yml`) | every push | Builds and runs the test image. |
| `Docker` (`docker-publish.yml`) | version tags, PRs | Builds the production image; on tags pushes it to `ghcr.io` and signs it with cosign. |
| `Docs` (`docs.yml`) | push and PR to `master` | Stages the documentation and builds the site with `zensical build --strict`. |
