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
  Headers = Request#request.header,
  AcceptHeader = parse_http:get_header("Accept", Request, undefined),
  %%logging:debug("~p", [Headers]),
  MCPVersion = parse_http:get_header("Mcp-Protocol-Version", Request, "2025-03-26"),
  MCPSessionID = parse_http:get_header("Mcp-Session-Id", Request, none),
  IsInitializeRequest = is_initialize_request(Request#request.body),
  ContentType = parse_http:get_header("Content-Type", Request, none),
  {AcceptHeader, MCPVersion, MCPSessionID, IsInitializeRequest, ContentType}.

handle_mcp_session_init(Response, Tool, KeepAliveMs, DecodedRequest) ->
  RequestId = maps:get(<<"id">>, DecodedRequest),
  case RequestId of
    none -> {aborted, 422};
    _ ->
      MCPSessionID = mcp_port:start_mcp_session(Tool, KeepAliveMs),
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
        RequestId)
  end.

wait_for_sse(SessionPid, Response, RequestID) ->
  receive
    terminate -> {aborted, 504};
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
  wait_for_sse(SessionPid, Response, maps:get(<<"id">>, DecodedRequest)).

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

mcp_stream_loop(Upstream, Response) ->
  receive
    terminate ->
      logging:debug("SSE stream terminated"),
      Upstream ! close, %% return keep-alive timer back
      {sent, Response};
    {data, Data} ->
      Upstream ! {send, Data}
  end.

handle_mcp_stream(Response, MCPSessionID) ->
  case mcp_port:get_mcp_session_by_id(list_to_binary(MCPSessionID)) of
    none ->
      logging:warn("MCP session ID ~p not found", [MCPSessionID]),
      {aborted, 404};
    _ ->
      Upstream = Response#response.upstream,
      Upstream ! cancel_tmr, %% We handle keep alive
      UpdatedResponse = response:set_header(Response, "Content-Type", "text/event-stream"),
      Upstream ! {send, response:response_headers(UpdatedResponse#response.headers, 200)},
      mcp_server:connect_to_mcp_session(MCPSessionID),
      mcp_stream_loop(Upstream, Response)
  end.
is_initialize_request(Body) when is_binary(Body) ->
  binary:match(Body, <<"\"method\"">>) =/= nomatch andalso
    binary:match(Body, <<"\"initialize\"">>) =/= nomatch;
is_initialize_request(Body) when is_list(Body) ->
  is_initialize_request(list_to_binary(Body));
is_initialize_request(_) ->
  false.

rule_mcp_call(_, _, {_, MCPVersion, _, _, _}, _) when MCPVersion < ?mcp_version ->
  logging:warn("Too old MCP version requested: ~p", [MCPVersion]),
  {aborted, 501};
rule_mcp_call(<<"POST">>, _, {_, _, MCPSessionID, IsInitializeRequest, _}, _)
  when MCPSessionID == none, not IsInitializeRequest -> {aborted, 400};
rule_mcp_call(<<"POST">>, _, {_, _, _, _, ContentType}, _)
  when ContentType /= "application/json" -> {aborted, 415};
rule_mcp_call(<<"POST">>, [Tool, KeepAliveMs], {_, _, _, IsInitializeRequest, _}, Response)
  when IsInitializeRequest ->
  {ok, ResponseWithFullRequest} = handle:handle_body_recv(Response),
  %%logging:debug("~p", [ResponseWithFullRequest]),
  DecodedRequest = json:decode(ResponseWithFullRequest#response.request#request.body),
  handle_mcp_session_init(ResponseWithFullRequest, Tool,
    list_to_integer(KeepAliveMs), DecodedRequest);

rule_mcp_call(<<"POST">>, [_, _], {_, _, MCPSessionID, _, _}, Response) ->
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

rule_mcp_call(<<"DELETE">>, _, {_, _, MCPSessionID, _, _}, Response) ->
  case mcp_port:get_mcp_session_by_id(list_to_binary(MCPSessionID)) of
    none ->
      logging:warn("MCP session ID ~p not found", [MCPSessionID]),
      {aborted, 404};
    {ok, Pid} ->
      logging:debug("Terminating ~p", [Pid]),
      mcp_server:terminate_session(Pid),
      {ready2send, Response#response{body = <<>>, code = 204}}
  end;

rule_mcp_call(Method, _, _, _) ->
  logging:debug("Method not allowed ~p", [Method]),
  {aborted, 405}.

rule_mcp_streamable_call(Args, Response) ->
  Method = Response#response.request#request.method,

  rule_mcp_call(Method, Args,
    mcp_request_state(Response#response.request), Response).
