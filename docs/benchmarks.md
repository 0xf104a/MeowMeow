# Benchmark results

Measured by the `Benchmark` workflow, which runs [`bench/run.sh`](development/benchmarking.md)
on a GitHub-hosted runner for every release tag and records the summary under
`bench/history/`. This page is generated from those files by
`python3 bench/report.py docs bench/history docs/benchmarks.md`; edit the generator,
not the page.

Shared runners differ from run to run, so compare releases by the order of magnitude
and by `Red./req`, the reductions the server VM spent per request, which does not
depend on how busy the runner was. Throughput and latency are indicative only.

No release has been benchmarked yet. The first tag pushed after the workflow
was added will fill this page.

*Generated on 2026-10-23 from 0 recorded release(s).*
