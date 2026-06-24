%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sophie Kasierocka
%%% @doc
%%%  Module which provides Host and If-Header rules.
%%% @end
%%%-------------------------------------------------------------------
-module(header).
-author("f104a").
-behaviour(nya_module).
-include("../../request.hrl").
-include("../../response.hrl").
-include("../../config.hrl").

-export([init/0, terminate/1, get_custom_rules/0, get_context_rules/0]).

init() -> ok.

terminate(_State) -> ok.

get_custom_rules() -> [].

get_context_rules() ->
  [
    {"If-Header", fun(Args, Request) -> context_if_header(Args, Request) end},
    {"Host", fun(Args, Request) -> context_host(Args, Request) end}
  ].

context_if_header([HeaderName], Request) ->
  parse_http:get_header(HeaderName, Request,  none) /= none;
context_if_header([HeaderName, HeaderValue], Request) ->
  case maps:find(HeaderName, Request#request.header) of
    {ok, HeaderValue} -> true;
    _ -> false
  end;
context_if_header(_, _) -> false.

context_host([Pattern], Request) ->

  Host = parse_http:get_header("Host", Request, ""),
  util:check_wildcard(Host, Pattern);
context_host(_, _) -> false.
