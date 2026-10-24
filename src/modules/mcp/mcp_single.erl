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

-export([rule_mcp_sse_single_call/2, init_table/0]).

-define(ROUTE_TABLE, mcp_route_sessions).
-define(TABLE_OWNER, mcp_route_sessions_owner).

%% @doc
%% Makes sure the route -> session table exists. ETS tables die with their
%% owner, so it is never created by a (short-lived) connection handler but
%% by a dedicated keeper process that lives as long as the VM.
%% @end
init_table() ->
  case ets:info(?ROUTE_TABLE) of
    undefined ->
      Self = self(),
      Keeper = spawn(fun() ->
                       catch register(?TABLE_OWNER, self()),
                       catch ets:new(?ROUTE_TABLE, [named_table, public, set]),
                       Self ! {?ROUTE_TABLE, ready},
                       receive stop -> ok end
                     end),
      receive
        {?ROUTE_TABLE, ready} -> ok
      after 5000 ->
        exit(Keeper, kill),
        {error, timeout}
      end;
    _ -> ok
  end.

get_route(Request) ->
  Request#request.route.

new_session(Route, Tool, KeepAliveMs, Opts) ->
  case mcp_port:start_mcp_session(Tool, KeepAliveMs, Opts) of
    {ok, MCPSessionID} ->
      {ok, Pid} = mcp_port:get_mcp_session_by_id(MCPSessionID),
      Session = #{ pid => Pid, initialized => false, init_response => <<>> },
      ets:insert(?ROUTE_TABLE, {Route, Session}),
      {ok, Session};
    {error, _} = Error ->
      Error
  end.

%% @doc The route's live session, or a new one: {ok, Session} | {error, busy | term()}.
get_or_create_session(Route, Tool, KeepAliveMs, Opts) ->
  case ets:lookup(?ROUTE_TABLE, Route) of
    [{Route, Session}] ->
      Pid = maps:get(pid, Session),
      IsAlive = erlang:is_process_alive(Pid),
      logging:debug("IsAlive(Pid ~p)=~p", [Pid, IsAlive]),
      case IsAlive of
        true -> {ok, Session};
        _ ->
          logging:debug("Will create new session for tool ~p", [Tool]),
          new_session(Route, Tool, KeepAliveMs, Opts)
      end;
    [] ->
      logging:debug("No session for route ~p,  will start ~p", [Route, Tool]),
      new_session(Route, Tool, KeepAliveMs, Opts)
  end.

update_session(Route, Session) ->
  ets:insert(?ROUTE_TABLE, {Route, Session}).

delete_session(Route) ->
  ets:delete(?ROUTE_TABLE, Route).

handle_sse_open_single(Response, Tool, KeepAliveMs, Opts) ->
  init_table(),

  Request = Response#response.request,
  Route = get_route(Request),

  case get_or_create_session(Route, Tool, KeepAliveMs, Opts) of
    {error, busy} -> mcp_options:busy_response(Response, mcp_options:session_opts(Opts));
    {error, _} -> {aborted, 502};
    {ok, Session} -> open_single_stream(Response, Request, Session)
  end.

open_single_stream(Response, Request, Session) ->
  logging:debug("Got session: ~p", [Session]),
  SessionPid = maps:get(pid, Session),
  Upstream = Response#response.upstream,
  Upstream ! cancel_tmr,

  UpdatedResponse =
    response:set_header(Response, "Content-Type", "text/event-stream"),

  Upstream ! {send,
    response:response_headers(UpdatedResponse#response.headers, 200)},

  logging:debug("Ready to connect to MCP"),
  mcp_server:connect_to_mcp_session(SessionPid),
  logging:debug("Connected to MCP"),
  Upstream ! {send, mcp_sse:format_sse_event("endpoint", Request#request.route)},

  mcp_sse:sse_push_loop(Upstream, Response, SessionPid),
  {sent, Response}.


handle_post_single(Response, Tool, KeepAliveMs, Opts) ->
  init_table(),

  {ok, ResponseWithBody} = handle:handle_body_recv(Response),
  Request = ResponseWithBody#response.request,
  Route = get_route(Request),

  case get_or_create_session(Route, Tool, KeepAliveMs, Opts) of
    {error, busy} -> mcp_options:busy_response(ResponseWithBody, mcp_options:session_opts(Opts));
    {error, _} -> {aborted, 502};
    {ok, Session} -> forward_single_post(ResponseWithBody, Route, Session)
  end.

forward_single_post(ResponseWithBody, Route, Session) ->
  Body = ResponseWithBody#response.request#request.body,
  SessionPid = maps:get(pid, Session),
  Initialized = maps:get(initialized, Session),

  case mcp_sse:is_initialize_request(Body) of
    true when Initialized =:= true ->
      InitResp = maps:get(init_response, Session),
      mcp_server:notify_mcp_session(SessionPid, InitResp),
      ok;

    true ->
      %% first initialize passes through
      mcp_server:notify_mcp_session(SessionPid, Body),
      update_session(Route, Session#{ initialized => true, init_response => Body });

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

rule_mcp_sse_single(<<"GET">>, {Tool, KeepAliveMs, Opts}, _, Response) ->
  handle_sse_open_single(Response, Tool, KeepAliveMs, Opts);

rule_mcp_sse_single(<<"POST">>, {Tool, KeepAliveMs, Opts}, _, Response) ->
  handle_post_single(Response, Tool, KeepAliveMs, Opts);

rule_mcp_sse_single(<<"DELETE">>, _, _, Response) ->
  handle_delete_single(Response);

rule_mcp_sse_single(Method, _, _, _) ->
  logging:debug("SSE single: method not allowed ~p", [Method]),
  {aborted, 405}.

rule_mcp_sse_single_call(Args, Response) ->
  Method = Response#response.request#request.method,
  mcp_options:with_rule_args(Args,
    fun(Tool, KeepAliveMs, Opts) ->
      rule_mcp_sse_single(Method, {Tool, KeepAliveMs, Opts},
        mcp_sse:mcp_request_state(Response#response.request), Response)
    end).

