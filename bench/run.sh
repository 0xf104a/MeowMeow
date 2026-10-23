#!/usr/bin/env bash
# MeowMeow HTTP benchmark driver.
#
# Builds the working tree into the benchmark image (bench/Dockerfile), starts
# it in a container and runs wrk against it, one scenario of
# bench/scenarios.tsv at a time. Results land in bench/results/<label>/:
#
#   meta.json          what was measured, where and with which settings
#   <scenario>.json    wrk summary plus VM counters before and after the run
#   <scenario>.wrk.txt wrk's own output
#   summary.md/.json   written by bench/report.py when python3 is available
#
# Compare two runs with: python3 bench/report.py compare <base dir> <new dir>
set -euo pipefail

BENCH_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_DIR=$(cd "$BENCH_DIR/.." && pwd)
SCENARIOS_FILE="$BENCH_DIR/scenarios.tsv"

DURATION=10
WARMUP=2
CONNECTIONS=64
THREADS=$(( $(nproc 2>/dev/null || echo 2) / 2 ))
[ "$THREADS" -lt 1 ] && THREADS=1
PORT=8089
IMAGE=bench.meow.meow
LABEL=""
OUT_ROOT="$BENCH_DIR/results"
CLIENT=auto
NETWORK=host
SERVER_CPUS=""
CLIENT_CPUS=""
BUILD=1
KEEP=0
RUNTIME="${CONTAINER_RUNTIME:-}"
LOGLEVEL="${MEOW_LOGLEVEL:-1}"
SELECTED=()

usage() {
  cat <<EOF
Usage: bench/run.sh [options] [scenario ...]

Builds the working tree into the benchmark image, starts it in a container
and runs wrk against it, one scenario at a time. Without scenario names every
scenario of bench/scenarios.tsv runs.

Options:
  -d, --duration SEC     measured time per scenario (default: $DURATION)
  -w, --warmup SEC       unmeasured warm-up run per scenario (default: $WARMUP)
  -c, --connections N    open connections (default: $CONNECTIONS)
  -t, --threads N        wrk threads (default: $THREADS)
  -p, --port PORT        port the server listens on (default: $PORT)
  -o, --out DIR          results root (default: bench/results)
  -l, --label NAME       results directory name (default: <date>-<git sha>)
      --client host|container
                         where wrk runs: on the host (needs wrk in PATH) or
                         inside the server container (default: host when wrk
                         is installed, container otherwise)
      --network host|bridge
                         container network. bridge publishes the port through
                         the runtime's forwarder, which costs throughput, so
                         host is the default
      --server-cpus LIST pin the server container to these CPUs (cpuset)
      --client-cpus LIST pin a host wrk to these CPUs (taskset)
      --runtime NAME     container runtime (default: \$CONTAINER_RUNTIME,
                         then docker, then podman)
      --image NAME       image tag (default: $IMAGE)
      --no-build         reuse the existing image
      --keep             leave the server container running afterwards
      --list             list scenarios and exit
  -h, --help             this text
EOF
}

log()  { printf '=> %s\n' "$*" >&2; }
die()  { printf 'error: %s\n' "$*" >&2; exit 1; }

while [ $# -gt 0 ]; do
  case "$1" in
    -d|--duration)    DURATION=$2; shift 2 ;;
    -w|--warmup)      WARMUP=$2; shift 2 ;;
    -c|--connections) CONNECTIONS=$2; shift 2 ;;
    -t|--threads)     THREADS=$2; shift 2 ;;
    -p|--port)        PORT=$2; shift 2 ;;
    -o|--out)         OUT_ROOT=$2; shift 2 ;;
    -l|--label)       LABEL=$2; shift 2 ;;
    --client)         CLIENT=$2; shift 2 ;;
    --network)        NETWORK=$2; shift 2 ;;
    --server-cpus)    SERVER_CPUS=$2; shift 2 ;;
    --client-cpus)    CLIENT_CPUS=$2; shift 2 ;;
    --runtime)        RUNTIME=$2; shift 2 ;;
    --image)          IMAGE=$2; shift 2 ;;
    --no-build)       BUILD=0; shift ;;
    --keep)           KEEP=1; shift ;;
    --list)
      awk -F'\t' '!/^#/ && NF {printf "%-14s %-18s %s\n", $1, $2, $6}' "$SCENARIOS_FILE"
      exit 0 ;;
    -h|--help)        usage; exit 0 ;;
    --)               shift; SELECTED+=("$@"); break ;;
    -*)               die "unknown option $1 (try --help)" ;;
    *)                SELECTED+=("$1"); shift ;;
  esac
done

case "$NETWORK" in host|bridge) ;; *) die "--network must be host or bridge" ;; esac
case "$CLIENT" in auto|host|container) ;; *) die "--client must be host or container" ;; esac

# ---------------------------------------------------------------- runtime

if [ -z "$RUNTIME" ]; then
  if command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
    RUNTIME=docker
  elif command -v podman >/dev/null 2>&1; then
    RUNTIME=podman
  else
    die "neither docker nor podman found; set --runtime"
  fi
fi
command -v "$RUNTIME" >/dev/null 2>&1 || die "container runtime '$RUNTIME' not found"

if [ "$CLIENT" = auto ]; then
  if command -v wrk >/dev/null 2>&1; then CLIENT=host; else CLIENT=container; fi
fi
if [ "$CLIENT" = host ]; then
  command -v wrk >/dev/null 2>&1 || die "--client host needs wrk in PATH"
  command -v curl >/dev/null 2>&1 || die "--client host needs curl in PATH"
fi

TASKSET=()
if [ -n "$CLIENT_CPUS" ]; then
  [ "$CLIENT" = host ] || die "--client-cpus only applies to --client host"
  command -v taskset >/dev/null 2>&1 || die "--client-cpus needs taskset"
  TASKSET=(taskset -c "$CLIENT_CPUS")
fi

GIT_SHA=$(git -C "$REPO_DIR" rev-parse --short HEAD 2>/dev/null || echo unknown)
GIT_DIRTY=""
git -C "$REPO_DIR" diff --quiet 2>/dev/null || GIT_DIRTY=".dirty"
[ -n "$LABEL" ] || LABEL="$(date +%Y%m%d-%H%M%S)-${GIT_SHA}${GIT_DIRTY}"
OUT_DIR="$OUT_ROOT/$LABEL"
NAME="meowbench-$$"
URL_BASE="http://127.0.0.1:$PORT"

# Scenario names given on the command line must exist.
for s in "${SELECTED[@]:-}"; do
  [ -z "$s" ] && continue
  awk -F'\t' -v n="$s" '!/^#/ && $1 == n {found = 1} END {exit !found}' "$SCENARIOS_FILE" \
    || die "unknown scenario '$s' (see --list)"
done

# ------------------------------------------------------------------ helpers

# Runs a command where the load generator lives (host or server container).
client_exec() {
  if [ "$CLIENT" = host ]; then
    # The ${arr[@]+...} form keeps bash 3.2 from tripping over an empty array under set -u.
    ${TASKSET[@]+"${TASKSET[@]}"} "$@"
  else
    "$RUNTIME" exec "$NAME" "$@"
  fi
}

# Prints a JSON string literal for $1.
json_str() {
  printf '"%s"' "$(printf '%s' "$1" | sed -e 's/\\/\\\\/g' -e 's/"/\\"/g')"
}

vm_stats() {
  "$RUNTIME" exec "$NAME" meowbench stats 2>/dev/null || echo null
}

cleanup() {
  if [ "$KEEP" = 1 ]; then
    log "server container $NAME left running (stop it with: $RUNTIME rm -f $NAME)"
  else
    "$RUNTIME" rm -f "$NAME" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

wait_ready() {
  local i
  for i in $(seq 1 60); do
    if client_exec curl -fs -o /dev/null "$URL_BASE/generate_204" 2>/dev/null; then
      return 0
    fi
    if [ "$("$RUNTIME" container inspect -f '{{.State.Running}}' "$NAME" 2>/dev/null)" != true ]; then
      "$RUNTIME" logs "$NAME" 2>&1 | tail -n 20 >&2 || true
      die "server container exited before it became ready (see the log above)"
    fi
    sleep 0.5
  done
  "$RUNTIME" logs "$NAME" 2>&1 | tail -n 20 >&2 || true
  die "server did not answer on $URL_BASE within 30 s"
}

# run_wrk SECONDS PATH HEADERS EXPECT EXTRA -> wrk output on stdout
run_wrk() {
  local seconds=$1 path=$2 headers=$3 expect=$4 extra=$5
  local lua args=()
  if [ "$CLIENT" = host ]; then lua="$BENCH_DIR/wrk/report.lua"; else lua=/opt/bench/report.lua; fi
  args=(-t "$THREADS" -c "$CONNECTIONS" -d "${seconds}s" --latency -s "$lua")
  if [ "$extra" != "-" ]; then
    # shellcheck disable=SC2206  # word-splitting of the extra arguments is intended
    args+=($extra)
  fi
  args+=("$URL_BASE$path")
  client_exec env "BENCH_HEADERS=$headers" "BENCH_EXPECT=$expect" wrk "${args[@]}"
}

# ------------------------------------------------------------------- build

cd "$REPO_DIR"
if [ "$BUILD" = 1 ]; then
  log "building $IMAGE with $RUNTIME (git ${GIT_SHA}${GIT_DIRTY})"
  "$RUNTIME" build -t "$IMAGE" -f "$BENCH_DIR/Dockerfile" \
    --build-arg "GIT_SHA=$GIT_SHA" --build-arg "GIT_DIRTY=$GIT_DIRTY" . >&2
else
  "$RUNTIME" image inspect "$IMAGE" >/dev/null 2>&1 || die "image $IMAGE not found; drop --no-build"
fi

# ------------------------------------------------------------------- start

RUN_ARGS=(-d --name "$NAME" -e "MEOW_PORT=$PORT" -e "MEOW_LOGLEVEL=$LOGLEVEL")
if [ "$NETWORK" = host ]; then
  RUN_ARGS+=(--network host)
else
  RUN_ARGS+=(-p "127.0.0.1:$PORT:$PORT")
fi
[ -n "$SERVER_CPUS" ] && RUN_ARGS+=(--cpuset-cpus "$SERVER_CPUS")

# The server binds without SO_REUSEADDR, so connections a previous run left
# in TIME_WAIT on the host (conn_close produces thousands) make the bind fail.
if [ "$NETWORK" = host ] && command -v ss >/dev/null 2>&1; then
  lingering=$(ss -Htan "( sport = :$PORT )" 2>/dev/null | wc -l)
  if [ "$lingering" -gt 0 ]; then
    log "warning: $lingering sockets still use port $PORT (TIME_WAIT from a previous run?);"
    log "         if the server fails to bind, wait a minute or pick another port with -p"
  fi
fi

log "starting server container $NAME (network $NETWORK, port $PORT)"
"$RUNTIME" run "${RUN_ARGS[@]}" "$IMAGE" serve >/dev/null
wait_ready

mkdir -p "$OUT_DIR"
SERVER_VM=$(vm_stats)
CPU_MODEL=$(grep -m1 'model name' /proc/cpuinfo 2>/dev/null | sed 's/.*: //' || true)

cat > "$OUT_DIR/meta.json" <<EOF
{
  "label": $(json_str "$LABEL"),
  "date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "git_sha": $(json_str "$GIT_SHA"),
  "git_dirty": $([ -n "$GIT_DIRTY" ] && echo true || echo false),
  "git_branch": $(json_str "$(git -C "$REPO_DIR" rev-parse --abbrev-ref HEAD 2>/dev/null || echo unknown)"),
  "runtime": $(json_str "$RUNTIME"),
  "image": $(json_str "$IMAGE"),
  "client": $(json_str "$CLIENT"),
  "network": $(json_str "$NETWORK"),
  "server_cpus": $(json_str "$SERVER_CPUS"),
  "client_cpus": $(json_str "$CLIENT_CPUS"),
  "duration_s": $DURATION,
  "warmup_s": $WARMUP,
  "connections": $CONNECTIONS,
  "threads": $THREADS,
  "log_level": $LOGLEVEL,
  "host": {
    "kernel": $(json_str "$(uname -sr)"),
    "cpus": $(nproc 2>/dev/null || echo 0),
    "cpu_model": $(json_str "$CPU_MODEL"),
    "wrk": $(json_str "$(client_exec wrk --version 2>&1 | head -n 1 || true)")
  },
  "server_vm": $SERVER_VM
}
EOF

log "results: $OUT_DIR"
log "settings: ${DURATION}s per scenario, $CONNECTIONS connections, $THREADS threads, client on $CLIENT"

# --------------------------------------------------------------- scenarios

FAILED=0
ORDER=0
while IFS=$'\t' read -r name path expect headers extra description; do
  case "$name" in ''|'#'*) continue ;; esac
  ORDER=$((ORDER + 1))
  if [ "${#SELECTED[@]}" -gt 0 ]; then
    keep=0
    for s in "${SELECTED[@]}"; do [ "$s" = "$name" ] && keep=1; done
    [ "$keep" = 1 ] || continue
  fi

  printf '\n== %s: %s\n' "$name" "$description" >&2
  if [ "$WARMUP" -gt 0 ]; then
    run_wrk "$WARMUP" "$path" "$headers" "$expect" "$extra" >/dev/null 2>&1 || true
  fi

  before=$(vm_stats)
  if ! output=$(run_wrk "$DURATION" "$path" "$headers" "$expect" "$extra" 2>&1); then
    printf '%s\n' "$output" >&2
    log "wrk failed for $name"
    FAILED=1
    continue
  fi
  after=$(vm_stats)

  printf '%s\n' "$output" | grep -v '^BENCHJSON ' > "$OUT_DIR/$name.wrk.txt"
  wrk_json=$(printf '%s\n' "$output" | sed -n 's/^BENCHJSON //p' | tail -n 1)
  if [ -z "$wrk_json" ]; then
    printf '%s\n' "$output" >&2
    log "no BENCHJSON line in wrk output for $name"
    FAILED=1
    continue
  fi

  cat > "$OUT_DIR/$name.json" <<EOF
{
  "scenario": $(json_str "$name"),
  "order": $ORDER,
  "description": $(json_str "$description"),
  "path": $(json_str "$path"),
  "headers": $(json_str "$headers"),
  "extra_args": $(json_str "$extra"),
  "expected_status": $expect,
  "duration_s": $DURATION,
  "connections": $CONNECTIONS,
  "threads": $THREADS,
  "wrk": $wrk_json,
  "vm_before": $before,
  "vm_after": $after
}
EOF
  printf '%s\n' "$output" | grep -E '^(Requests/sec|Transfer/sec|  *Latency  |  *Socket errors|  *Non-2xx)' >&2
done < "$SCENARIOS_FILE"

# ------------------------------------------------------------------ report

if command -v python3 >/dev/null 2>&1; then
  echo >&2
  python3 "$BENCH_DIR/report.py" summarize "$OUT_DIR"
else
  log "python3 not found: skipping summary.md (the per-scenario JSON files are complete)"
fi

[ "$FAILED" = 0 ] || die "some scenarios failed"
