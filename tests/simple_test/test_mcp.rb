require "net/http"
require "uri"
require "json"

# MeowMeow fronts tests/simple_mcp/server.rb on /mcp with MCP-Tool-Streamable
# (protocol revision 2025-11-25). These tests drive the transport both as a
# 2025-11-25 client and as a 2026-07-28 ("modern", stateless) client.
#
# In 2026-07-28 terms MeowMeow is a *legacy* server, so what matters for a
# modern client is the fallback contract in
# https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http#backward-compatibility
# a 400 whose body is NOT a recognised modern JSON-RPC error means "fall back
# to the initialize handshake"; a JSON-RPC error body would mean "this server
# is modern, retry with one of its supported versions" and the client would
# never reach initialize.
module Tests
  MCP_LEGACY = "2025-11-25"
  MCP_MODERN = "2026-07-28"
  MCP_META = {
    "io.modelcontextprotocol/protocolVersion" => MCP_MODERN,
    "io.modelcontextprotocol/clientInfo" => { "name" => "simple_test", "version" => "0" },
    "io.modelcontextprotocol/clientCapabilities" => {}
  }.freeze

  # Helpers must not start with `test_`: SimpleTest runs every such method.

  def self.mcp_send(req, headers)
    headers.each { |k, v| req[k] = v }
    Net::HTTP.start("localhost", 80) { |http| http.request(req) }
  end

  def self.mcp_post(body, headers = {})
    req = Net::HTTP::Post.new(URI("http://localhost/mcp"))
    req["Content-Type"] = "application/json"
    req["Accept"] = "application/json, text/event-stream"
    req.body = body.is_a?(String) ? body : JSON.generate(body)
    self.mcp_send(req, headers)
  end

  def self.mcp_get(headers = {})
    req = Net::HTTP::Get.new(URI("http://localhost/mcp"))
    req["Accept"] = "application/json, text/event-stream"
    self.mcp_send(req, headers)
  end

  def self.mcp_delete(headers = {})
    self.mcp_send(Net::HTTP::Delete.new(URI("http://localhost/mcp")), headers)
  end

  # A JSON-RPC request the way a 2026-07-28 client writes it: `_meta` on every call.
  def self.mcp_modern(method, params = {}, id = 1)
    { "jsonrpc" => "2.0", "id" => id, "method" => method, "params" => params.merge("_meta" => MCP_META) }
  end

  def self.mcp_modern_headers(method, name = nil)
    h = { "Mcp-Protocol-Version" => MCP_MODERN, "Mcp-Method" => method }
    h["Mcp-Name"] = name unless name.nil?
    h
  end

  # True when a dual-era client would NOT read body as a modern JSON-RPC error.
  def self.mcp_not_jsonrpc_error?(body)
    parsed = JSON.parse(body.to_s)
    !(parsed.is_a?(Hash) && parsed.key?("jsonrpc"))
  rescue JSON::ParserError
    true
  end

  def self.mcp_initialize(version_header)
    body = {
      "jsonrpc" => "2.0", "id" => 1, "method" => "initialize",
      "params" => {
        "protocolVersion" => MCP_LEGACY, "capabilities" => {},
        "clientInfo" => { "name" => "simple_test", "version" => "0" }
      }
    }
    self.mcp_post(body, { "Mcp-Protocol-Version" => version_header })
  end

  def self.mcp_session_headers(sid, version = MCP_LEGACY)
    { "Mcp-Protocol-Version" => version, "Mcp-Session-Id" => sid }
  end

  # Session teardown is asynchronous; wait (bounded) until the id is gone.
  def self.mcp_wait_gone(sid)
    20.times do
      res = self.mcp_post({ "jsonrpc" => "2.0", "id" => 9, "method" => "ping" }, self.mcp_session_headers(sid))
      return true if res.code == "404"
      sleep 0.1
    end
    false
  end

  # ==========================================
  # 2026-07-28 client, first contact (no session)
  # ==========================================

  def self.test_mcp_modern_discover_probe_falls_back
    res = self.mcp_post(self.mcp_modern("server/discover", {}, "discover-1"),
                        self.mcp_modern_headers("server/discover"))
    res.code == "400" && self.mcp_not_jsonrpc_error?(res.body)
  end

  def self.test_mcp_modern_tools_call_without_session_is_400
    body = self.mcp_modern("tools/call", { "name" => "echo", "arguments" => { "text" => "hi" } })
    res = self.mcp_post(body, self.mcp_modern_headers("tools/call", "echo"))
    res.code == "400" && self.mcp_not_jsonrpc_error?(res.body)
  end

  def self.test_mcp_modern_notification_without_session_is_400
    body = { "jsonrpc" => "2.0", "method" => "notifications/cancelled",
             "params" => { "requestId" => 1, "_meta" => MCP_META } }
    res = self.mcp_post(body, self.mcp_modern_headers("notifications/cancelled"))
    res.code == "400"
  end

  def self.test_mcp_modern_version_header_is_not_refused
    res = self.mcp_initialize(MCP_MODERN)
    sid = res["Mcp-Session-Id"]
    ok = res.code == "201" && !sid.nil? && !sid.empty?
    self.mcp_delete(self.mcp_session_headers(sid)) unless sid.nil?
    ok
  end

  def self.test_mcp_old_version_header_is_400
    res = self.mcp_post({ "jsonrpc" => "2.0", "id" => 1, "method" => "ping" },
                        { "Mcp-Protocol-Version" => "2025-06-18", "Mcp-Session-Id" => "nope" })
    res.code == "400" && self.mcp_not_jsonrpc_error?(res.body)
  end

  def self.test_mcp_get_without_session_is_400
    self.mcp_get({ "Mcp-Protocol-Version" => MCP_MODERN }).code == "400"
  end

  def self.test_mcp_delete_without_session_is_400
    self.mcp_delete({ "Mcp-Protocol-Version" => MCP_MODERN }).code == "400"
  end

  # ==========================================
  # Dual-era client after the fallback: a 2025-11-25 session
  # ==========================================

  def self.test_mcp_legacy_round_trip
    init = self.mcp_initialize(MCP_LEGACY)
    return false unless init.code == "201"
    sid = init["Mcp-Session-Id"]
    return false if sid.nil? || sid.empty?
    return false unless JSON.parse(init.body).dig("result", "protocolVersion") == MCP_LEGACY

    h = self.mcp_session_headers(sid)
    begin
      # notifications/initialized: forwarded, 202, nothing to read back
      note = self.mcp_post({ "jsonrpc" => "2.0", "method" => "notifications/initialized" }, h)
      return false unless note.code == "202" && note.body.to_s.empty?

      # a request decorated the 2026-07-28 way still reaches the tool
      ping = self.mcp_post(self.mcp_modern("ping", {}, 2), h.merge("Mcp-Method" => "ping"))
      return false unless ping.code == "200" && JSON.parse(ping.body)["result"] == {}

      call = self.mcp_post(self.mcp_modern("tools/call", { "name" => "echo", "arguments" => { "text" => "hi" } }, 3),
                           h.merge("Mcp-Method" => "tools/call", "Mcp-Name" => "echo"))
      return false unless call.code == "200"
      return false unless JSON.parse(call.body).dig("result", "content", 0, "text") == "Echo: hi"

      # server/discover is relayed to the legacy tool, which says Method not found
      disc = self.mcp_post(self.mcp_modern("server/discover", {}, 4), h)
      return false unless disc.code == "200" && JSON.parse(disc.body).dig("error", "code") == -32601
    ensure
      del = self.mcp_delete(h)
    end
    del.code == "204" && self.mcp_wait_gone(sid)
  end

  def self.test_mcp_unknown_session_is_404
    res = self.mcp_post({ "jsonrpc" => "2.0", "id" => 1, "method" => "ping" }, self.mcp_session_headers("unknown"))
    res.code == "404"
  end
end
