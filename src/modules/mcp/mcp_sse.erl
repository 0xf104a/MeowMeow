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

-export([rule_mcp_sse_call/2, sse_push_loop/3, mcp_request_state/1, is_initialize_request/1, format_sse_event/2]).

%% @doc True when Body is a JSON-RPC request whose method is "initialize".
%% A body that is not (yet) valid JSON - it may still be arriving - falls back
%% to a substring check so that partially received initialize requests are
%% still recognised.
is_initialize_request(Body) when is_binary(Body) ->
  try json:decode(Body) of
    #{<<"method">> := <<"initialize">>} -> true;
    _ -> false
  catch
    _:_ ->
      binary:match(Body, <<"\"method\"">>) =/= nomatch andalso
        binary:match(Body, <<"\"initialize\"">>) =/= nomatch
  end;
is_initialize_request(Body) when is_list(Body) ->
  is_initialize_request(list_to_binary(Body));
is_initialize_request(_) -> false.

mcp_request_state(Request) ->
  MCPSessionID = parse_http:get_query_parameter("sessionId", Request, none),
  IsInitializeRequest = is_initialize_request(Request#request.body),
  ContentType = parse_http:media_type(parse_http:get_header("Content-Type", Request, none)),
  {MCPSessionID, IsInitializeRequest, ContentType}.

%% Client opened SSE stream without session — create new session + subprocess,
%% then emit endpoint event so client knows where to POST
handle_sse_open(Response, Tool, KeepAliveMs, Opts, SessionAt) ->
  case mcp_port:start_mcp_session(Tool, KeepAliveMs, Opts) of
    {error, busy} -> mcp_options:busy_response(Response, mcp_options:session_opts(Opts));
    {error, _} -> {aborted, 502};
    {ok, MCPSessionID} -> open_sse_stream(Response, MCPSessionID, SessionAt)
  end.

open_sse_stream(Response, MCPSessionID, SessionAt) ->
  Upstream = Response#response.upstream,
  Upstream ! cancel_tmr,

  %% Send SSE headers
  UpdatedResponse = response:set_header(Response, "Content-Type", "text/event-stream"),
  Upstream ! {send, response:response_headers(UpdatedResponse#response.headers, 200)},

  %% Emit endpoint event — this is what n8n waits for
  %% Client will POST all messages to /sse?sessionId=<id>
  PostUrl = [SessionAt, "?sessionId=", MCPSessionID],
  Upstream ! {send, format_sse_event("endpoint", PostUrl)},

  %% Register as receiver and loop
  {ok, SessionPid} = mcp_port:get_mcp_session_by_id(MCPSessionID),
  mcp_server:connect_to_mcp_session(SessionPid),
  sse_push_loop(Upstream, Response, SessionPid).

sse_push_loop(Upstream, Response, SessionPid) ->
  receive
    terminated ->
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

rule_mcp_sse(<<"GET">>, {Tool, KeepAliveMs, Opts}, {none, _, _}, Response) ->
  %% No sessionId on GET — fresh connection, create session
  handle_sse_open(Response, Tool, KeepAliveMs, Opts, Response#response.request#request.route);

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
  mcp_options:with_rule_args(Args,
    fun(Tool, KeepAliveMs, Opts) ->
      rule_mcp_sse(Method, {Tool, KeepAliveMs, Opts},
        mcp_request_state(Response#response.request), Response)
    end).