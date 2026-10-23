-- wrk script used by bench/run.sh.
--
--  * BENCH_HEADERS  extra request headers, "Name: value;Other: value"
--  * BENCH_EXPECT   the HTTP status every response must carry; any other
--                   status is counted in "unexpected_status"
--
-- When the run ends a single line "BENCHJSON {...}" is printed with the
-- throughput, latency distribution and error counters, which run.sh stores
-- next to wrk's human-readable output.

local extra = os.getenv("BENCH_HEADERS")
if extra and extra ~= "" and extra ~= "-" then
  for pair in string.gmatch(extra, "[^;]+") do
    local name, value = string.match(pair, "^%s*([^:]+):%s*(.-)%s*$")
    if name then wrk.headers[name] = value end
  end
end

local expect = tonumber(os.getenv("BENCH_EXPECT") or "")
local unexpected = 0

if expect then
  response = function(status, headers, body)
    if status ~= expect then unexpected = unexpected + 1 end
  end
end

done = function(summary, latency, requests)
  local seconds = summary.duration / 1e6
  local e = summary.errors
  io.write(string.format(
    'BENCHJSON {"duration_us":%d,"requests":%d,"bytes":%d,'
    .. '"requests_per_sec":%.2f,"bytes_per_sec":%.2f,'
    .. '"latency_us":{"min":%d,"max":%d,"mean":%.2f,"stdev":%.2f,'
    .. '"p50":%d,"p75":%d,"p90":%d,"p99":%d,"p999":%d},'
    .. '"errors":{"connect":%d,"read":%d,"write":%d,"status":%d,"timeout":%d},'
    .. '"unexpected_status":%d,"expected_status":%s}\n',
    summary.duration, summary.requests, summary.bytes,
    summary.requests / seconds, summary.bytes / seconds,
    latency.min, latency.max, latency.mean, latency.stdev,
    latency:percentile(50), latency:percentile(75), latency:percentile(90),
    latency:percentile(99), latency:percentile(99.9),
    e.connect, e.read, e.write, e.status, e.timeout,
    unexpected, expect and tostring(expect) or "null"))
end
