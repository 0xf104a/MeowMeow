%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%   MCP streamable API implementation as in 2025-11-25 protocol specifications
%%% @end
%%% @see https://modelcontextprotocol.io/specification/2025-11-25/basic/transports
%%%-------------------------------------------------------------------
-module(mcp_streamable).
-author("f104a").
-include("../../response.hrl").
-include("../../request.hrl").
-include("mcp.hrl").

%% API
-export([rule_mcp_streamable_call/2]).

mcp_request_state(Request) ->
  AcceptHeader = parse_http:get_header("Accept", Request, undefined),
  %% The header is only mandatory after initialization; when absent the
  %% version is negotiated in the initialize exchange, so 'none' is not rejected.
  MCPVersion = parse_http:get_header("Mcp-Protocol-Version", Request, none),
  MCPSessionID = parse_http:get_header("Mcp-Session-Id", Request, none),
  IsInitializeRequest = mcp_sse:is_initialize_request(Request#request.body),
  ContentType = parse_http:media_type(parse_http:get_header("Content-Type", Request, none)),
  {AcceptHeader, MCPVersion, MCPSessionID, IsInitializeRequest, ContentType}.

handle_mcp_session_init(Response, Tool, KeepAliveMs, Opts, DecodedRequest) ->
  RequestId = maps:get(<<"id">>, DecodedRequest, none),
  case RequestId of
    none -> {aborted, 422};
    _ ->
      %% Starting the tool and waiting for its initialize result can take
      %% longer than the connection's keep-alive; that timer is for idle
      %% connections, not for requests in flight. handle:handler/3 re-arms it
      %% once the response is out.
      Response#response.upstream ! cancel_tmr,
      case mcp_port:start_mcp_session(Tool, KeepAliveMs, Opts) of
        {error, busy} -> mcp_options:busy_response(Response, mcp_options:session_opts(Opts));
        {error, _} -> {aborted, 502};
        {ok, MCPSessionID} -> start_session_stream(Response, MCPSessionID, RequestId)
      end
  end.

start_session_stream(Response, MCPSessionID, RequestId) ->
      {ok, SessionPid} = mcp_port:get_mcp_session_by_id(MCPSessionID),

      logging:debug("New SessionPid ~p", [SessionPid]),
      %% Register ourselves as receiver
      mcp_server:connect_to_mcp_session(SessionPid),

      %% Forward the original request body to subprocess stdin
      RawBody = Response#response.request#request.body,
      mcp_server:notify_mcp_session(SessionPid, RawBody),
      wait_for_sse(SessionPid, Response#response{
        code = 201,
        headers = maps:merge(Response#response.headers,
          #{"Content-Type" => "application/json", "MCP-Session-Id" => binary_to_list(MCPSessionID)}
        )},
        RequestId).

wait_for_sse(SessionPid, Response, RequestID) ->
  receive
    terminated -> {aborted, 504};
    Data ->
      logging:debug("~p", [Data]),
      Decoded = json:decode(Data),
      IsTargetID = maps:get(<<"id">>, Decoded) == RequestID,
      case IsTargetID of
        true ->
          mcp_server:disconnect_mcp_session(SessionPid),
          {ready2send,
            Response#response{is_ready2send = true,
              body = Data,
              headers = maps:merge(Response#response.headers,
                #{"Content-Type" => "application/json"})
            }
          };
        _ -> wait_for_sse(SessionPid, Response, RequestID)
      end
  end.

handle_mcp_sse_call(_, none, _) ->
  logging:err("SSE call with none session!"),
  {aborted, 404};

handle_mcp_sse_call(Response, SessionPid, DecodedRequest) ->
  Response#response.upstream ! cancel_tmr, %% Disable keep-alive timeouts
  logging:debug("Connecting to ~p", [SessionPid]),
  mcp_server:connect_to_mcp_session(SessionPid),
  logging:debug("Notifying ~p with ~p", [SessionPid, Response#response.request#request.body]),
  mcp_server:notify_mcp_session(SessionPid, Response#response.request#request.body),
  %% The handler starts every response as a 404 (handle:get_default_response/2);
  %% a relayed tool answer is a 200.
  wait_for_sse(SessionPid, Response#response{code = 200}, maps:get(<<"id">>, DecodedRequest)).

handle_mcp_notification(Response, SessionPid) ->
  logging:debug("Notifying ~p", [SessionPid]),
  mcp_server:notify_mcp_session(SessionPid, Response#response.request#request.body),
  logging:debug("Notify ok"),
  {ready2send, Response#response{body = <<"">>, code = 202}}.

handle_mcp_call(_, none, _) ->
  logging:err("MCP call with none session!"),
  {aborted, 404};
handle_mcp_call(Response, MCPSessionID, DecodedRequest) ->
  HasIDKey = maps:is_key(<<"id">>, DecodedRequest),
  {ok, SessionPid} = mcp_port:get_mcp_session_by_id(list_to_binary(MCPSessionID)),
  case HasIDKey of
    true -> handle_mcp_sse_call(Response, SessionPid, DecodedRequest);
    false -> handle_mcp_notification(Response, SessionPid)
  end.

%% Server-initiated messages are relayed as SSE events until the session ends.
mcp_stream_loop(Upstream, Response) ->
  receive
    terminated ->
      logging:debug("SSE stream terminated"),
      Upstream ! close, %% return keep-alive timer back
      {sent, Response};
    Data when is_binary(Data) ->
      Upstream ! {send, mcp_sse:format_sse_event("message", Data)},
      mcp_stream_loop(Upstream, Response)
  end.

handle_mcp_stream(Response, MCPSessionID) ->
  case mcp_port:get_mcp_session_by_id(list_to_binary(MCPSessionID)) of
    none ->
      logging:warn("MCP session ID ~p not found", [MCPSessionID]),
      {aborted, 404};
    {ok, SessionPid} ->
      Upstream = Response#response.upstream,
      Upstream ! cancel_tmr, %% We handle keep alive
      UpdatedResponse = response:set_header(Response, "Content-Type", "text/event-stream"),
      Upstream ! {send, response:response_headers(UpdatedResponse#response.headers, 200)},
      mcp_server:connect_to_mcp_session(SessionPid),
      mcp_stream_loop(Upstream, Response)
  end.

%% An unsupported protocol version is a 400 (2025-11-25 and 2026-07-28 both
%% require it). The body stays the plain HTML error page: a dual-era
%% 2026-07-28 client falls back to the initialize handshake only when a 400
%% carries no recognised modern JSON-RPC error, so this must not become a
%% JSON-RPC -32022 error until a stateless (2026-07-28) path exists.
rule_mcp_call(_, _, {_, MCPVersion, _, _, _}, _)
  when MCPVersion =/= none, MCPVersion < ?mcp_version ->
  logging:warn("Unsupported MCP version requested: ~p", [MCPVersion]),
  {aborted, 400};
%% A session-less POST that is not initialize. This is also what a 2026-07-28
%% client's first (stateless) request looks like; the 400 tells it to fall back.
rule_mcp_call(<<"POST">>, _, {_, _, MCPSessionID, IsInitializeRequest, _}, _)
  when MCPSessionID == none, not IsInitializeRequest -> {aborted, 400};
rule_mcp_call(<<"POST">>, _, {_, _, _, _, ContentType}, _)
  when ContentType /= "application/json" -> {aborted, 415};
rule_mcp_call(<<"POST">>, {Tool, KeepAliveMs, Opts}, {_, _, _, IsInitializeRequest, _}, Response)
  when IsInitializeRequest ->
  {ok, ResponseWithFullRequest} = handle:handle_body_recv(Response),
  %%logging:debug("~p", [ResponseWithFullRequest]),
  DecodedRequest = json:decode(ResponseWithFullRequest#response.request#request.body),
  handle_mcp_session_init(ResponseWithFullRequest, Tool, KeepAliveMs, Opts, DecodedRequest);

rule_mcp_call(<<"POST">>, _, {_, _, MCPSessionID, _, _}, Response) ->
  case mcp_port:get_mcp_session_by_id(list_to_binary(MCPSessionID)) of
    none ->
      logging:warn("MCP session ID ~p not found", [MCPSessionID]),
      {aborted, 404};
    _ ->
      {ok, ResponseAfterRecv} = handle:handle_body_recv(Response),
      %%logging:debug("~p", [ResponseAfterRecv]),
      RequestWithBody = ResponseAfterRecv#response.request,
      DecodedRequest = json:decode(RequestWithBody#request.body),
      %%logging:debug("DecodedRequest=~p", [DecodedRequest]),
      handle_mcp_call(Response#response{request = RequestWithBody}, MCPSessionID, DecodedRequest)
  end;

rule_mcp_call(<<"GET">>, _, {_, _, MCPSessionID, _, _}, Response) ->
  IsJsonAcceptable = parse_http:acceptable_for_request("application/json", Response#response.request),
  IsSSEStreamAcceptable = parse_http:acceptable_for_request("text/event-stream", Response#response.request),
  if MCPSessionID == none -> {aborted, 400};
    (not IsJsonAcceptable) or (not IsSSEStreamAcceptable) -> {aborted, 406};
    true -> handle_mcp_stream(Response, MCPSessionID)
  end;

rule_mcp_call(<<"DELETE">>, _, {_, _, none, _, _}, _) -> {aborted, 400};
rule_mcp_call(<<"DELETE">>, _, {_, _, MCPSessionID, _, _}, Response) ->
  SessionId = list_to_binary(MCPSessionID),
  case mcp_port:get_mcp_session_by_id(SessionId) of
    none ->
      logging:warn("MCP session ID ~p not found", [MCPSessionID]),
      {aborted, 404};
    {ok, Pid} ->
      logging:debug("Terminating ~p", [Pid]),
      %% terminate_mcp_session/1 gives the tool a 300 ms SIGTERM grace period,
      %% which must not count against the keep-alive timer (see above). It also
      %% forgets the id, so the next request with it is a 404 right away.
      Response#response.upstream ! cancel_tmr,
      mcp_port:terminate_mcp_session(SessionId),
      {ready2send, Response#response{body = <<>>, code = 204}}
  end;

rule_mcp_call(Method, _, _, _) ->
  logging:debug("Method not allowed ~p", [Method]),
  {aborted, 405}.

rule_mcp_streamable_call(Args, Response) ->
  Method = Response#response.request#request.method,
  mcp_options:with_rule_args(Args,
    fun(Tool, KeepAliveMs, Opts) ->
      rule_mcp_call(Method, {Tool, KeepAliveMs, Opts},
        mcp_request_state(Response#response.request), Response)
    end).
