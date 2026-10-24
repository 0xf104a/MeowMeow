require 'json'

# Simple MCP server in Ruby over stdio: a *legacy* (protocol revision
# 2025-11-25) tool, so the E2E suite can check how MeowMeow fronts it for
# both 2025-11-25 and 2026-07-28 clients. JSON-RPC 2.0, one message per line.

$stdout.sync = true

TOOLS = [
  {
    name: 'echo',
    description: 'Echoes back the input',
    inputSchema: { type: 'object', properties: { text: { type: 'string' } } }
  }
].freeze

def reply(id, result)
  puts({ jsonrpc: '2.0', id: id, result: result }.to_json)
end

def fail_with(id, code, message)
  puts({ jsonrpc: '2.0', id: id, error: { code: code, message: message } }.to_json)
end

loop do
  line = gets
  break if line.nil?

  begin
    request = JSON.parse(line)
  rescue JSON::ParserError
    next
  end

  id = request['id']
  method = request['method']

  # A notification has no id and never gets an answer (JSON-RPC 2.0 section 4.1).
  next if id.nil?

  case method
  when 'initialize'
    reply(id, {
      protocolVersion: '2025-11-25',
      capabilities: { tools: { listChanged: false } },
      serverInfo: { name: 'simple-ruby-mcp', version: '0.2.0' }
    })
  when 'ping'
    reply(id, {})
  when 'tools/list'
    reply(id, { tools: TOOLS })
  when 'tools/call'
    if request.dig('params', 'name') == 'echo'
      text = request.dig('params', 'arguments', 'text')
      reply(id, { content: [{ type: 'text', text: "Echo: #{text}" }] })
    else
      fail_with(id, -32602, "Unknown tool: #{request.dig('params', 'name')}")
    end
  else
    # Also what a legacy tool says to a 2026-07-28 `server/discover` probe:
    # the stdio Backward Compatibility rules expect an implementation-defined
    # error such as this one, and the client then falls back to initialize.
    fail_with(id, -32601, "Method not found: #{method}")
  end
end
