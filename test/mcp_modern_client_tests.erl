%%%-------------------------------------------------------------------
%%% @doc EUnit suite: MeowMeow as a *legacy* Streamable HTTP server facing
%%% 2026-07-28 ("modern", stateless) MCP clients.
%%%
%%% Spec: https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http
%%%       https://modelcontextprotocol.io/specification/2026-07-28/basic/versioning
%%%
%%% MeowMeow implements revision 2025-11-25, which 2026-07-28 calls a legacy
%%% (initialize-based) server. The spec's compatibility contract for that
%%% case is what this suite pins down:
%%%
%%%  - a dual-era client first sends a modern, session-less request carrying
%%%    `_meta.io.modelcontextprotocol/protocolVersion' and the Mcp-Method /
%%%    Mcp-Name headers. A 400 whose body is NOT a recognised modern JSON-RPC
%%%    error tells it to fall back to the initialize handshake; a JSON-RPC
%%%    error body (UnsupportedProtocolVersion -32022, HeaderMismatch -32020)
%%%    would instead make it believe the server is modern and stop;
%%%  - after falling back it keeps working under 2025-11-25 semantics, and
%%%    the newer version header, the extra headers or `_meta' keys must not
%%%    break the gateway;
%%%  - an unsupported protocol version is a 400, never a 501.
%%%
%%% `cat' plays the tool so forwarded bodies come back verbatim.
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_modern_client_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/request.hrl").
-include("../src/response.hrl").

-define(MODERN, "2026-07-28").
-define(LEGACY, "2025-11-25").
-define(ACCEPT, {"Accept", "application/json, text/event-stream"}).

setup() ->
  test_support:setup_all(#{"LogLevel" => "0"}),
  test_support:setup_mcp(),
  ok.

cleanup(_) ->
  test_support:cleanup_mcp_sessions(),
  test_support:teardown_all(ok).

cat() -> test_support:tool("cat").
args() -> [cat(), "5000"].

%% The `_meta' block every 2026-07-28 request carries.
meta() ->
  "\"_meta\":{\"io.modelcontextprotocol/protocolVersion\":\"" ++ ?MODERN ++ "\","
  "\"io.modelcontextprotocol/clientInfo\":{\"name\":\"eunit\",\"version\":\"0\"},"
  "\"io.modelcontextprotocol/clientCapabilities\":{}}".

discover() ->
  "{\"jsonrpc\":\"2.0\",\"id\":\"discover-1\",\"method\":\"server/discover\",\"params\":{" ++ meta() ++ "}}".

tools_call() ->
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\","
  "\"arguments\":{\"text\":\"hi\"}," ++ meta() ++ "}}".

cancelled() ->
  "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/cancelled\",\"params\":{\"requestId\":1," ++ meta() ++ "}}".

ping() ->
  "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\",\"params\":{" ++ meta() ++ "}}".

initialize() ->
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"" ++ ?LEGACY ++ "\","
  "\"capabilities\":{},\"clientInfo\":{\"name\":\"eunit\",\"version\":\"0\"}}}".

%% Headers a 2026-07-28 client puts on every POST (Mcp-Name only on tools/call,
%% resources/read and prompts/get).
modern_headers(Method) -> [{"Mcp-Protocol-Version", ?MODERN}, {"Mcp-Method", Method}, ?ACCEPT].
modern_headers(Method, Name) -> [{"Mcp-Name", Name} | modern_headers(Method)].

json_post(Headers, Body) ->
  test_support:request("POST", "/mcp",
                       [{"Host", "h"}, {"Content-Type", "application/json"},
                        {"Content-Length", integer_to_list(length(Body))} | Headers], Body).

%% Starts from the response handle:handle/2 really hands to the rule chain
%% (a 404 with the error page), so the status a client sees is checked too.
call(Req) ->
  Upstream = test_support:spawn_collector(),
  try
    Resp = (test_support:response_for(Req, Upstream))#response{code = 404, body = <<"<html>404</html>">>},
    mcp_streamable:rule_mcp_streamable_call(args(), Resp)
  after
    test_support:stop_collector(Upstream)
  end.

new_session() ->
  Id = mcp_port:start_mcp_session(cat(), 5000),
  {ok, Pid} = mcp_port:get_mcp_session_by_id(Id),
  {binary_to_list(Id), Pid}.

with_session(Fun) ->
  {Id, Pid} = new_session(),
  try Fun(Id)
  after catch mcp_server:terminate_session(Pid)
  end.

mcp_modern_client_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(_) -> [probe_tests(), fallback_tests()] end}.

%%--------------------------------------------------------------------
%% The modern client's first contact: session-less requests
%%--------------------------------------------------------------------
probe_tests() ->
  [{"a 2026-07-28 server/discover probe without a session is a 400 (spec 2026-07-28 Backward "
    "Compatibility: on a 400 without a recognised modern JSON-RPC error body the client falls back "
    "to initialize), not a 405, 415 or 502",
    ?_assertEqual({aborted, 400}, call(json_post(modern_headers("server/discover"), discover())))},
   {"a 2026-07-28 tools/call carrying Mcp-Method and Mcp-Name but no session is a 400: the "
    "unknown headers are ignored rather than rejected",
    ?_assertEqual({aborted, 400}, call(json_post(modern_headers("tools/call", "echo"), tools_call())))},
   {"a 2026-07-28 notification (no id) without a session is a 400, not a 202: nothing is "
    "forwarded when there is no tool to forward to",
    ?_assertEqual({aborted, 400}, call(json_post(modern_headers("notifications/cancelled"), cancelled())))},
   {"Mcp-Protocol-Version 2026-07-28 is newer than 2025-11-25 and is not refused as unsupported",
    ?_assertNotEqual({aborted, 400},
                     call(json_post([{"Mcp-Protocol-Version", ?MODERN}, {"Mcp-Session-Id", "X"}], ping())))},
   {"the 400 error page is not a JSON-RPC message, so a dual-era client does fall back (a body "
    "with a 'jsonrpc' key and error -32022/-32020 would mark the server as modern and stop the fallback)",
    ?_test(begin
             {400, Headers, Body} = test_support:parse_response(handle:abort(400)),
             Bin = iolist_to_binary(Body),
             ?assertNotEqual(nomatch, binary:match(Bin, <<"<html>">>)),
             ?assertEqual(nomatch, binary:match(Bin, <<"jsonrpc">>)),
             ?assertError(_, json:decode(Bin)),
             ?assertNotEqual("application/json", maps:get("Content-Type", Headers, undefined))
           end)},
   {"a 2026-07-28 client never sends GET or DELETE; a legacy server still answers a session-less "
    "GET with 400 (a modern-only server would say 405)",
    ?_assertEqual({aborted, 400},
                  call(test_support:request("GET", "/mcp", [{"Host", "h"}, {"Mcp-Protocol-Version", ?MODERN}, ?ACCEPT])))},
   {"a session-less DELETE from a client that gave up on a session is a 400 as well",
    ?_assertEqual({aborted, 400},
                  call(test_support:request("DELETE", "/mcp", [{"Host", "h"}, {"Mcp-Protocol-Version", ?MODERN}])))}].

%%--------------------------------------------------------------------
%% After the fallback: legacy semantics with modern decorations
%%--------------------------------------------------------------------
fallback_tests() ->
  [{"a fallback initialize that still carries Mcp-Protocol-Version 2026-07-28 opens a session: "
    "201, Mcp-Session-Id, the tool's answer as JSON",
    {timeout, 15, ?_test(begin
       {Tag, R} = call(json_post([{"Mcp-Protocol-Version", ?MODERN}, ?ACCEPT], initialize())),
       ?assertEqual(ready2send, Tag),
       ?assertEqual(201, R#response.code),
       Sid = maps:get("MCP-Session-Id", R#response.headers),
       ?assert(is_list(Sid) andalso Sid =/= ""),
       ?assertEqual("application/json", maps:get("Content-Type", R#response.headers)),
       ?assertEqual(list_to_binary(initialize()), iolist_to_binary(R#response.body)),
       {ok, Pid} = mcp_port:get_mcp_session_by_id(list_to_binary(Sid)),
       catch mcp_server:terminate_session(Pid)
     end)}},
   {"inside a session, a request decorated the 2026-07-28 way (version header, Mcp-Method, "
    "Mcp-Name, `_meta' in params) is forwarded to the tool verbatim and answered as JSON",
    {timeout, 15, ?_test(with_session(fun(Sid) ->
       {Tag, R} = call(json_post([{"Mcp-Session-Id", Sid} | modern_headers("tools/call", "echo")], tools_call())),
       ?assertEqual(ready2send, Tag),
       ?assertEqual(200, R#response.code),
       ?assertEqual("application/json", maps:get("Content-Type", R#response.headers)),
       Echoed = iolist_to_binary(R#response.body),
       ?assertEqual(list_to_binary(tools_call()), Echoed),
       #{<<"params">> := #{<<"_meta">> := Meta}} = json:decode(Echoed),
       ?assertEqual(list_to_binary(?MODERN), maps:get(<<"io.modelcontextprotocol/protocolVersion">>, Meta))
     end))}},
   {"inside a session, server/discover is not intercepted by the gateway: a legacy tool gets to "
    "answer it itself (2026-07-28 stdio Backward Compatibility expects an implementation-defined error from it)",
    {timeout, 15, ?_test(with_session(fun(Sid) ->
       {Tag, R} = call(json_post([{"Mcp-Session-Id", Sid} | modern_headers("server/discover")], discover())),
       ?assertEqual(ready2send, Tag),
       ?assertEqual(200, R#response.code),
       ?assertEqual(list_to_binary(discover()), iolist_to_binary(R#response.body))
     end))}},
   {"inside a session, a notification decorated with `_meta' is accepted with 202 and no body",
    {timeout, 15, ?_test(with_session(fun(Sid) ->
       {Tag, R} = call(json_post([{"Mcp-Session-Id", Sid} | modern_headers("notifications/cancelled")], cancelled())),
       ?assertEqual(ready2send, Tag),
       ?assertEqual(202, R#response.code),
       ?assertEqual(<<>>, iolist_to_binary(R#response.body))
     end))}},
   {"inside a session, a version older than 2025-11-25 is a 400 (not 501) even with a valid session",
    {timeout, 15, ?_test(with_session(fun(Sid) ->
       ?assertEqual({aborted, 400},
                    call(json_post([{"Mcp-Protocol-Version", "2025-06-18"}, {"Mcp-Session-Id", Sid}], ping())))
     end))}}].
