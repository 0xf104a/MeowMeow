#!/bin/sh
# Entry point of the benchmark image (bench/Dockerfile).
#
#   meowbench serve        start the server in the foreground (default)
#   meowbench shell        start the server as a daemon, then open a shell
#   meowbench stats        print VM counters of the running server as JSON
#   meowbench wrk ARGS...  run the bundled wrk
#
# MEOW_PORT (default 8089) and MEOW_LOGLEVEL (default 1) are written into
# /etc/nya/meow.conf before the server starts.
set -e

REL=/opt/MeowMeow/bin/MeowMeow
PORT="${MEOW_PORT:-8089}"
LOGLEVEL="${MEOW_LOGLEVEL:-1}"

configure() {
  sed -i \
    -e "s/^ListenPort .*/ListenPort $PORT/" \
    -e "s/^LogLevel .*/LogLevel $LOGLEVEL/" \
    /etc/nya/meow.conf
}

# One tuple so the VM never prints it as a string (a list of small integers
# would be); the fields are turned into a JSON object below. nodetool wants
# a complete form, hence the trailing full stop.
STATS_EXPR='{erlang:memory(total), erlang:memory(processes), erlang:memory(binary), erlang:system_info(process_count), erlang:system_info(port_count), erlang:statistics(run_queue), element(1, erlang:statistics(reductions)), erlang:system_info(schedulers_online), erlang:system_info(otp_release)}.'

case "${1:-serve}" in
  serve)
    configure
    exec "$REL" foreground
    ;;
  shell)
    configure
    "$REL" daemon
    exec /bin/sh
    ;;
  stats)
    raw=$("$REL" eval "$STATS_EXPR" 2>/dev/null | tr -d ' \n')
    case "$raw" in
      '{'[0-9]*) ;;
      *) echo "meowbench stats: unexpected reply from the node: $raw" >&2; echo null; exit 1 ;;
    esac
    printf '%s' "$raw" | tr -d '{}' | awk -F, '{
      printf "{\"memory_total\":%s,\"memory_processes\":%s,\"memory_binary\":%s,", $1, $2, $3
      printf "\"process_count\":%s,\"port_count\":%s,\"run_queue\":%s,", $4, $5, $6
      printf "\"reductions\":%s,\"schedulers_online\":%s,\"otp_release\":%s}\n", $7, $8, $9
    }'
    ;;
  wrk)
    shift
    exec wrk "$@"
    ;;
  *)
    echo "usage: meowbench [serve|shell|stats|wrk ARGS...]" >&2
    exit 2
    ;;
esac
