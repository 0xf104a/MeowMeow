# Benchmarking

`bench/` holds a load-test harness for the HTTP server. It builds the working
tree into a container image together with [wrk](https://github.com/wg/wrk),
starts the server and runs a fixed list of scenarios against it. Docker or
Podman is the only requirement.

```bash
bench/run.sh                 # build the image, run every scenario, print a table
bench/run.sh --list          # the scenarios and what each one measures
bench/run.sh -d 30 -c 256 static_64k no_content
```

The last line of every run names the results directory,
`bench/results/<date>-<git sha>/`. It contains `summary.md` with the table
below, one JSON file per scenario with wrk's numbers and the server VM's
counters, and `meta.json` describing the host and the settings.

## Scenarios

Each scenario requests one path of `bench/config/routes.conf` over
keep-alive connections for a fixed time:

| Scenario       | Measures                                                 |
|----------------|----------------------------------------------------------|
| `no_content`   | parsing, routing and response assembly, no file I/O      |
| `index`        | `DocDir` lookup and a small HTML file                    |
| `static_1k`    | a static file smaller than one chunk                     |
| `static_64k`   | a static file of exactly one 64 KiB chunk                |
| `static_1m`    | streaming, 16 chunks                                     |
| `static_16m`   | streaming, 256 chunks, with 16 connections               |
| `sendfile_64k` | `Send-File` without the `DocDir` lookup                  |
| `headers`      | eight `Set-Header` rules before the response             |
| `deep_routes`  | 256 non-matching `Route` blocks before the match         |
| `not_found`    | the 404 path of `DocDir`                                 |
| `abort_418`    | the `Abort` directive                                    |
| `conn_close`   | one TCP connection per request (accept and process spawn) |

Add a scenario by adding a route to `bench/config/routes.conf` and a line to
`bench/scenarios.tsv` (name, path, expected status, extra request headers,
extra wrk arguments, description). Keep-alive scenarios must send
`Connection: keep-alive` explicitly: the server closes the connection after a
request without a `Connection` header, and wrk sends none by default.

## Reading the table

| Column     | Meaning                                                          |
|------------|------------------------------------------------------------------|
| Req/s      | completed requests per second                                    |
| MiB/s      | response bytes per second                                        |
| p50 … max  | wrk's per-request latency percentiles in milliseconds            |
| Red./req   | Erlang reductions the server VM spent per request                |
| VM MiB     | total VM memory right after the scenario                         |
| Procs      | VM process count right after the scenario                        |
| Errors     | socket errors and responses with a status other than expected    |

`Red./req` is the most stable column: it counts the server's own work and is
unaffected by the load generator sharing the machine. Throughput and latency
depend on how busy the host is.

## Comparing two revisions

Run the harness on both revisions with the same settings and compare the two
result directories:

```bash
git checkout master      && bench/run.sh -l base
git checkout my-branch   && bench/run.sh -l new
python3 bench/report.py compare bench/results/base bench/results/new
```

Changes of 5 % or more are marked (`+` better, `!` worse). To make the numbers
reproducible, pin the server and the client to disjoint CPU sets
(`--server-cpus 0-3 --client-cpus 4-7`, the latter needs `wrk` and `taskset`
on the host), keep the default `--network host`, and use at least ten seconds
per scenario.

## Where the load generator runs

With `wrk` installed on the host it runs there (`--client host`); otherwise
`run.sh` executes the copy bundled in the image (`--client container`), where
it shares the container's CPU budget with the server. Both talk to the server
over the loopback interface of the host network namespace. `--network bridge`
publishes the port instead, which measures the runtime's port forwarder as
much as the server and is only there for hosts that cannot use the host
network.

## In CI and in the documentation

The `Benchmark` workflow (`.github/workflows/benchmark.yml`) runs the harness
on a GitHub-hosted runner with a fresh server per scenario
(`--restart-each`), prints the table in the job summary and uploads the
results directory as an artifact. It runs on demand from the Actions tab and
on every release tag.

On a tag it also records the run: the summary is stored as
`bench/history/<tag>.json`, the [Benchmark results](../benchmarks.md) page is
regenerated from every recorded release, and both are committed to `master`
by the workflow, which Read the Docs then rebuilds as `latest`. The page is
generated, so edit `report.py` rather than the page:

```bash
python3 bench/report.py record bench/results/<label> bench/history <tag>
python3 bench/report.py docs bench/history docs/benchmarks.md
```

Shared runners are noisy, so treat those numbers as a smoke test and a trend
rather than a measurement; `Red./req` is the column that stays comparable.
