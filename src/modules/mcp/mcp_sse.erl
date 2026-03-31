%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%   MCP SSE (legacy) transport as in 2024-11-05 protocol specifications
%%% @end
%%% @see https://modelcontextprotocol.io/specification/2024-11-05/basic/transports
%%%-------------------------------------------------------------------
-module(mcp_sse).
-author("f104a").
-include("../../response.hrl").
-include("../../request.hrl").

-export([rule_mcp_sse_call/2]).

%% ── Helpers ──────────────────────────────────────────────────────────────────

is_initialize_request(Body) when is_binary(Body) ->
  binary:match(Body, <<"\"method\"">>) =/= nomatch andalso
    binary:match(Body, <<"\"initialize\"">>) =/= nomatch;
is_initialize_request(Body) when is_list(Body) ->
  is_initialize_request(list_to_binary(Body));
is_initialize_request(_) -> false.

mcp_request_state(Request) ->
  MCPSessionID = parse_http:get_query_parameter("sessionId", Request, none),
  IsInitializeRequest = is_initialize_request(Request#request.body),
  ContentType = parse_http:get_header("Content-Type", Request, none),
  {MCPSessionID, IsInitializeRequest, ContentType}.

%% ── GET /sse — open SSE stream, emit endpoint event ──────────────────────────

%% Client opened SSE stream without session — create new session + subprocess,
%% then emit endpoint event so client knows where to POST
handle_sse_open(Response, Tool, KeepAliveMs, SessionAt) ->
  MCPSessionID = mcp_port:start_mcp_session(Tool, list_to_integer(KeepAliveMs)),
  Upstream = Response#response.upstream,
  Upstream ! cancel_tmr,

  %% Send SSE headers
  UpdatedResponse = response:set_header(Response, "Content-Type", "text/event-stream"),
  Upstream ! {send, response:response_headers(UpdatedResponse#response.headers, 200)},

  %% Emit endpoint event — this is what n8n waits for
  %% Client will POST all messages to /sse?sessionId=<id>
  PostUrl = [SessionAt ++ "?sessionId=", binary_to_list(MCPSessionID)],
  Upstream ! {send, format_sse_event("endpoint", PostUrl)},

  %% Register as receiver and loop
  {ok, SessionPid} = mcp_port:get_mcp_session_by_id(MCPSessionID),
  mcp_server:connect_to_mcp_session(SessionPid),
  sse_push_loop(Upstream, Response, SessionPid).

sse_push_loop(Upstream, Response, SessionPid) ->
  receive
    terminate ->
      logging:debug("SSE stream terminated"),
      Upstream ! close,
      {sent, Response};
    Data when is_binary(Data) ->
      %% Wrap in SSE event frame and push down the stream
      Upstream ! {send, format_sse_event("message", Data)},
      sse_push_loop(Upstream, Response, SessionPid)
  end.

format_sse_event(EventType, Data) ->
  iolist_to_binary([
    "event: ", EventType, "\n",
    "data: ", Data, "\n\n"
  ]).

%% ── POST /sse?sessionId=<id> — receive message, return 202 ───────────────────

handle_sse_post(Response, MCPSessionID) ->
  {ok, ResponseWithBody} = handle:handle_body_recv(Response),
  Body = ResponseWithBody#response.request#request.body,
  case mcp_port:get_mcp_session_by_id(list_to_binary(MCPSessionID)) of
    none ->
      logging:warn("SSE POST: session ~p not found", [MCPSessionID]),
      {aborted, 404};
    {ok, SessionPid} ->
      logging:debug("SSE POST forwarding to ~p", [SessionPid]),
      mcp_server:notify_mcp_session(SessionPid, Body),
      %% Always 202 — response comes back via the GET stream
      {
        ready2send,
        response:set_header(
          ResponseWithBody#response{
            is_ready2send = true,
            code = 202,
            body = <<>>
          },
          "Content-Length", "0")
      }
  end.

%% ── Rule chain ───────────────────────────────────────────────────────────────

rule_mcp_sse(<<"GET">>, [Tool, KeepAliveMs, SessionAt], {none, _, _}, Response) ->
  %% No sessionId on GET — fresh connection, create session
  handle_sse_open(Response, Tool, KeepAliveMs, SessionAt);

rule_mcp_sse(<<"GET">>, _, {MCPSessionID, _, _}, Response)
  when MCPSessionID =/= none ->
  %% GET with existing sessionId — reconnect to existing session's push channel
  case mcp_port:get_mcp_session_by_id(list_to_binary(MCPSessionID)) of
    none ->
      logging:warn("SSE GET reconnect: session ~p not found", [MCPSessionID]),
      {aborted, 404};
    {ok, SessionPid} ->
      Upstream = Response#response.upstream,
      Upstream ! cancel_tmr,
      UpdatedResponse = response:set_header(Response, "Content-Type", "text/event-stream"),
      Upstream ! {send, response:response_headers(UpdatedResponse#response.headers, 200)},
      mcp_server:connect_to_mcp_session(SessionPid),
      sse_push_loop(Upstream, Response, SessionPid)
  end;

rule_mcp_sse(<<"POST">>, _, {none, _, _}, _) ->
  %% POST without sessionId — bad request
  {aborted, 400};

rule_mcp_sse(<<"POST">>, _, {_, _, ContentType}, _)
  when ContentType /= "application/json" ->
  {aborted, 415};

rule_mcp_sse(<<"POST">>, _, {MCPSessionID, _, _}, Response)
  when MCPSessionID =/= none ->
  handle_sse_post(Response, MCPSessionID);

rule_mcp_sse(<<"DELETE">>, _, {MCPSessionID, _, _}, Response)
  when MCPSessionID =/= none ->
  case mcp_port:get_mcp_session_by_id(list_to_binary(MCPSessionID)) of
    none -> {aborted, 404};
    {ok, Pid} ->
      mcp_server:terminate_session(Pid),
      {ready2send, Response#response{body = <<>>, code = 204}}
  end;

rule_mcp_sse(Method, _, _, _) ->
  logging:debug("SSE: method not allowed ~p", [Method]),
  {aborted, 405}.

rule_mcp_sse_call(Args, Response) ->
  Method = Response#response.request#request.method,
  rule_mcp_sse(Method, Args,
    mcp_request_state(Response#response.request), Response).