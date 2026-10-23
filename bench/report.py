#!/usr/bin/env python3
"""Summarise and compare MeowMeow benchmark runs produced by bench/run.sh.

    report.py summarize RESULTS_DIR          print a Markdown table, write
                                             summary.md and summary.json
    report.py compare BASE_DIR NEW_DIR       print a Markdown table of the
                                             change between two runs
"""
import json
import sys
from pathlib import Path

SKIP = {"meta.json", "summary.json"}


def load_run(directory):
    directory = Path(directory)
    if not directory.is_dir():
        sys.exit(f"error: {directory} is not a directory")
    meta_file = directory / "meta.json"
    meta = json.loads(meta_file.read_text()) if meta_file.exists() else {}
    results = []
    for path in directory.glob("*.json"):
        if path.name in SKIP:
            continue
        data = json.loads(path.read_text())
        if "wrk" in data:
            results.append(data)
    if not results:
        sys.exit(f"error: no scenario results in {directory}")
    # Keep the order of scenarios.tsv, which run.sh records as "order".
    results.sort(key=lambda d: (d.get("order", 0), d["scenario"]))
    return meta, {d["scenario"]: d for d in results}


def ms(us):
    return f"{us / 1000:.2f}"


def mib(per_sec):
    return f"{per_sec / (1024 * 1024):.1f}"


def errors(data):
    e = data["wrk"]["errors"]
    socket_errors = e["connect"] + e["read"] + e["write"] + e["timeout"]
    unexpected = data["wrk"].get("unexpected_status", 0)
    return socket_errors, unexpected


def reductions_per_request(data):
    before, after, requests = data.get("vm_before"), data.get("vm_after"), data["wrk"]["requests"]
    if not before or not after or not requests:
        return None
    return (after["reductions"] - before["reductions"]) / requests


def rows(scenarios):
    out = []
    for name, data in scenarios.items():
        w = data["wrk"]
        lat = w["latency_us"]
        socket_errors, unexpected = errors(data)
        rpr = reductions_per_request(data)
        after = data.get("vm_after") or {}
        out.append({
            "scenario": name,
            "requests": w["requests"],
            "requests_per_sec": w["requests_per_sec"],
            "bytes_per_sec": w["bytes_per_sec"],
            "p50_ms": lat["p50"] / 1000,
            "p90_ms": lat["p90"] / 1000,
            "p99_ms": lat["p99"] / 1000,
            "max_ms": lat["max"] / 1000,
            "reductions_per_request": rpr,
            "memory_total": after.get("memory_total"),
            "process_count": after.get("process_count"),
            "socket_errors": socket_errors,
            "unexpected_status": unexpected,
        })
    return out


def table(header, lines):
    widths = [max(len(str(c)) for c in col) for col in zip(header, *lines)]
    fmt = "| " + " | ".join(f"{{:<{w}}}" if i == 0 else f"{{:>{w}}}" for i, w in enumerate(widths)) + " |"
    sep = "|" + "|".join("-" * (w + 2) for w in widths) + "|"
    return "\n".join([fmt.format(*header), sep] + [fmt.format(*line) for line in lines])


def summarize(directory):
    meta, scenarios = load_run(directory)
    data = rows(scenarios)
    header = ["Scenario", "Req/s", "MiB/s", "p50 ms", "p90 ms", "p99 ms", "max ms",
              "Red./req", "VM MiB", "Procs", "Errors"]
    lines = []
    for r in data:
        err = ""
        if r["socket_errors"]:
            err += f"{r['socket_errors']} socket"
        if r["unexpected_status"]:
            err += (", " if err else "") + f"{r['unexpected_status']} status"
        lines.append([
            r["scenario"],
            f"{r['requests_per_sec']:,.0f}",
            mib(r["bytes_per_sec"]),
            f"{r['p50_ms']:.2f}", f"{r['p90_ms']:.2f}", f"{r['p99_ms']:.2f}", f"{r['max_ms']:.2f}",
            f"{r['reductions_per_request']:,.0f}" if r["reductions_per_request"] is not None else "-",
            f"{r['memory_total'] / (1024 * 1024):.1f}" if r["memory_total"] else "-",
            str(r["process_count"]) if r["process_count"] is not None else "-",
            err or "0",
        ])

    host = meta.get("host", {})
    vm = meta.get("server_vm") or {}
    title = f"# MeowMeow benchmark {meta.get('label', Path(directory).name)}"
    facts = [
        f"- Git: `{meta.get('git_sha', '?')}`{' (dirty)' if meta.get('git_dirty') else ''} on `{meta.get('git_branch', '?')}`, {meta.get('date', '?')}",
        f"- Load: {meta.get('duration_s', '?')} s per scenario, {meta.get('connections', '?')} connections, "
        f"{meta.get('threads', '?')} wrk threads, warm-up {meta.get('warmup_s', '?')} s, client on {meta.get('client', '?')}",
        f"- Server: OTP {vm.get('otp_release', '?')}, {vm.get('schedulers_online', '?')} schedulers, "
        f"LogLevel {meta.get('log_level', '?')}, {meta.get('runtime', '?')} with {meta.get('network', '?')} network"
        + (f", CPUs {meta['server_cpus']}" if meta.get("server_cpus") else ""),
        f"- Host: {host.get('cpu_model') or 'unknown CPU'}, {host.get('cpus', '?')} CPUs, {host.get('kernel', '?')}",
        "",
        "Latency columns are wrk's per-request latency. Red./req is the number of Erlang reductions "
        "the server VM spent per request, a CPU cost proxy that does not depend on the load generator. "
        "VM MiB and Procs are the VM's memory and process count right after the scenario.",
    ]
    text = "\n".join([title, ""] + facts + ["", table(header, lines), ""])
    print(text)
    (Path(directory) / "summary.md").write_text(text)
    (Path(directory) / "summary.json").write_text(json.dumps({"meta": meta, "scenarios": data}, indent=2) + "\n")


def delta(old, new, lower_is_better=False):
    if not old:
        return "-"
    change = (new - old) / old * 100
    mark = ""
    if abs(change) >= 5:
        improved = change < 0 if lower_is_better else change > 0
        mark = " +" if improved else " !"
    return f"{change:+.1f}%{mark}"


def compare(base_dir, new_dir):
    base_meta, base = load_run(base_dir)
    new_meta, new = load_run(new_dir)
    common = [name for name in base if name in new]
    if not common:
        sys.exit("error: the two runs share no scenario")
    header = ["Scenario", "Req/s base", "Req/s new", "Change", "p99 base", "p99 new", "Change", "Red./req base", "Red./req new"]
    lines = []
    for name in common:
        b, n = base[name]["wrk"], new[name]["wrk"]
        rb, rn = reductions_per_request(base[name]), reductions_per_request(new[name])
        lines.append([
            name,
            f"{b['requests_per_sec']:,.0f}", f"{n['requests_per_sec']:,.0f}",
            delta(b["requests_per_sec"], n["requests_per_sec"]),
            ms(b["latency_us"]["p99"]), ms(n["latency_us"]["p99"]),
            delta(b["latency_us"]["p99"], n["latency_us"]["p99"], lower_is_better=True),
            f"{rb:,.0f}" if rb is not None else "-", f"{rn:,.0f}" if rn is not None else "-",
        ])
    print(f"# {base_meta.get('label', base_dir)} -> {new_meta.get('label', new_dir)}")
    print()
    print("Changes of at least 5 % are marked: `+` better, `!` worse. Treat anything under "
          "that as noise unless the run was pinned to dedicated CPUs.")
    print()
    print(table(header, lines))
    missing = sorted(set(base) ^ set(new))
    if missing:
        print()
        print("Only in one run: " + ", ".join(missing))


def main(argv):
    if len(argv) == 3 and argv[1] == "summarize":
        summarize(argv[2])
    elif len(argv) == 4 and argv[1] == "compare":
        compare(argv[2], argv[3])
    else:
        sys.exit(__doc__.strip())


if __name__ == "__main__":
    main(sys.argv)
