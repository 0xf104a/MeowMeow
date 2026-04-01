%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%%   (inaudible screams directly from hell)
%%% @end
%%%-------------------------------------------------------------------
-module(mcp_single).
-author("f104a").
-include("../../response.hrl").
-include("../../request.hrl").

-export([rule_mcp_sse_single_call/2]).

-define(ROUTE_TABLE, mcp_route_sessions).

init_table() ->
  case ets:info(?ROUTE_TABLE) of
    undefined ->
      ets:new(?ROUTE_TABLE, [named_table, public, set]),
      ok;
    _ -> ok
  end.

get_route(Request) ->
  Request#request.route.

get_or_create_session(Route, Tool, KeepAliveMs) ->
  case ets:lookup(?ROUTE_TABLE, Route) of
    [{Route, Session}] ->
      Session;
    [] ->
      MCPSessionID = mcp_port:start_mcp_session(
        Tool,
        list_to_integer(KeepAliveMs)
      ),
      {ok, Pid} = mcp_port:get_mcp_session_by_id(MCPSessionID),

      Session = #{ pid => Pid, initialized => false },
      ets:insert(?ROUTE_TABLE, {Route, Session}),
      Session
  end.

update_session(Route, Session) ->
  ets:insert(?ROUTE_TABLE, {Route, Session}).

delete_session(Route) ->
  ets:delete(?ROUTE_TABLE, Route).

handle_sse_open_single(Response, Tool, KeepAliveMs) ->
  init_table(),

  Request = Response#response.request,
  Route = get_route(Request),

  Session = get_or_create_session(Route, Tool, KeepAliveMs),
  SessionPid = maps:get(pid, Session),

  Upstream = Response#response.upstream,
  Upstream ! cancel_tmr,

  UpdatedResponse =
    response:set_header(Response, "Content-Type", "text/event-stream"),

  Upstream ! {send,
    response:response_headers(UpdatedResponse#response.headers, 200)},

  mcp_server:connect_to_mcp_session(SessionPid),
  Upstream ! {send, mcp_sse:format_sse_event("endpoint", Request#request.route)},

  mcp_sse:sse_push_loop(Upstream, Response, SessionPid).

handle_post_single(Response, Tool, KeepAliveMs) ->
  init_table(),

  {ok, ResponseWithBody} = handle:handle_body_recv(Response),
  Request = ResponseWithBody#response.request,
  Route = get_route(Request),

  Body = Request#request.body,

  Session = get_or_create_session(Route, Tool, KeepAliveMs),
  SessionPid = maps:get(pid, Session),
  Initialized = maps:get(initialized, Session),

  case mcp_sse:is_initialize_request(Body) of
    true when Initialized =:= true ->
      %% swallow duplicate initialize like a black hole
      ok;

    true ->
      %% first initialize passes through
      mcp_server:notify_mcp_session(SessionPid, Body),
      update_session(Route, Session#{ initialized => true });

    false ->
      mcp_server:notify_mcp_session(SessionPid, Body)
  end,

  {
    ready2send,
    response:set_header(
      ResponseWithBody#response{
        is_ready2send = true,
        code = 202,
        body = <<>>
      },
      "Content-Length", "0")
  }.

handle_delete_single(Response) ->
  Request = Response#response.request,
  Route = get_route(Request),

  case ets:lookup(?ROUTE_TABLE, Route) of
    [] ->
      {aborted, 404};
    [{Route, Session}] ->
      Pid = maps:get(pid, Session),
      mcp_server:terminate_session(Pid),
      delete_session(Route),
      {ready2send, Response#response{body = <<>>, code = 204}}
  end.

rule_mcp_sse_single(<<"GET">>, [Tool, KeepAliveMs], _, Response) ->
  handle_sse_open_single(Response, Tool, KeepAliveMs);

rule_mcp_sse_single(<<"POST">>, [Tool, KeepAliveMs], _, Response) ->
  handle_post_single(Response, Tool, KeepAliveMs);

rule_mcp_sse_single(<<"DELETE">>, _, _, Response) ->
  handle_delete_single(Response);

rule_mcp_sse_single(Method, _, _, _) ->
  logging:debug("SSE single: method not allowed ~p", [Method]),
  {aborted, 405}.

rule_mcp_sse_single_call(Args, Response) ->
  Method = Response#response.request#request.method,
  rule_mcp_sse_single(Method, Args,
    mcp_sse:mcp_request_state(Response#response.request), Response).

