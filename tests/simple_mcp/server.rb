require 'json'

# Simple MCP server in Ruby using stdio
# Based on JSON-RPC 2.0 as expected by mcp.erl

$stdout.sync = true

loop do
  line = gets
  break if line.nil?

  begin
    request = JSON.parse(line)
    id = request['id']
    method = request['method']

    case method
    when 'initialize'
      response = {
        jsonrpc: '2.0',
        id: id,
        result: {
          protocolVersion: '2025-11-25',
          capabilities: {
            tools: {
              echo: {
                description: 'Echoes back the input',
                inputSchema: {
                  type: 'object',
                  properties: {
                    text: { type: 'string' }
                  }
                }
              }
            }
          },
          serverInfo: {
            name: 'simple-ruby-mcp',
            version: '0.1.0'
          }
        }
      }
      puts response.to_json
    when 'ping'
      response = {
        jsonrpc: '2.0',
        id: id,
        result: 'pong'
      }
      puts response.to_json
    else
      # Simple echo tool implementation if called
      if method == 'call_tool' && request.dig('params', 'name') == 'echo'
        text = request.dig('params', 'arguments', 'text')
        response = {
          jsonrpc: '2.0',
          id: id,
          result: {
            content: [{ type: 'text', text: "Echo: #{text}" }]
          }
        }
        puts response.to_json
      else
        # Generic unknown method response
        response = {
          jsonrpc: '2.0',
          id: id,
          error: {
            code: -32601,
            message: "Method not found: #{method}"
          }
        }
        puts response.to_json
      end
    end
  rescue JSON::ParserError
    # Ignore invalid JSON or send error
    # puts ({jsonrpc: '2.0', error: {code: -32700, message: 'Parse error'}}).to_json
  end
end
