%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sophie Kasierocka
%%% @doc
%%%  Authorization module
%%%  Provides:
%%%   - Context rule "Authorize" to guard nested rules by validating
%%%     bearer token from the HTTP Authorization header against a credentials file
%%%     that contains lines in form "username:password".
%%%     The bearer token is expected to be base64(username:password).
%%%     If header is missing or invalid, the context rule fails (returns false).
%%%
%%%   - Context rule "Authorize-Via" to authorize user via an external tool.
%%%     It calls the tool passing a header value to its stdin.
%%%     If the tool exits with code 0, user is authorized.
%%% @end
%%% Created : 24. черв. 2026 15:55
%%%-------------------------------------------------------------------
-module(authorization).
-author("f104a").
-behaviour(nya_module).
-include("../../request.hrl").
-include("../../response.hrl").
-include("../../config.hrl").

%% API
-export([init/0, terminate/1, get_custom_rules/0, get_context_rules/0]).

%%
%% Public API required by nya_module behaviour
%%
init() -> ok.

terminate(_State) -> ok.

get_custom_rules() ->
  [].

get_context_rules() ->
  [
    {"Authorize", fun(Args, Request) -> context_authorize(Args, Request) end},
    {"Authorize-Via", fun(Args, Request) -> context_authorize_via(Args, Request) end}
  ].

%%
%% Implementation
%%

%% Reads credentials from file: each non-empty, non-comment line is "username:password"
read_creds_lines(Dev, Acc) ->
  case file:read_line(Dev) of
    eof -> {ok, Acc};
    {ok, Line0} ->
      Line = string:trim(lists:nth(1, string:split(string:trim(Line0), "#"))),
      case length(Line) of
        0 -> read_creds_lines(Dev, Acc);
        _ -> read_creds_lines(Dev, Acc ++ [Line])
      end;
    Error -> Error
  end.

load_credentials(FilePath) ->
  case file:open(FilePath, read) of
    {ok, Dev} ->
      try
        {ok, Lines} = read_creds_lines(Dev, []),
        file:close(Dev),
        {ok, Lines}
      catch _:E ->
        file:close(Dev),
        logging:err("Failed to read credentials from ~p: ~p", [FilePath, E]),
        {error, read_error}
      end;
    {error, Reason} ->
      logging:err("Failed to open credentials file ~p: ~p", [FilePath, Reason]),
      {error, open_error}
  end.

%% Returns true if given base64 token corresponds to any "username:password" line
validate_bearer(TokenB64, Creds) ->
  case base64:decode(TokenB64) of
    Decoded when is_binary(Decoded) ->
      Str = binary_to_list(Decoded),
      lists:member(Str, Creds);
    _ -> false
  end.

%% Accept both Bearer and Basic schemes for convenience, but Bearer is primary per spec
extract_token(Request) ->
  Auth = parse_http:get_header("Authorization", Request, none),
  case Auth of
    none -> none;
    "" -> none;
    Val ->
      %% Normalize and split: "Scheme token"
      Parts = string:split(Val, " "),
      case Parts of
        [Scheme, Token] -> {string:lowercase(Scheme), Token};
        _ -> none
      end
  end.

%% Context rule: Authorize <CredFile>
context_authorize([CredFile], Request) ->
  case load_credentials(CredFile) of
    {ok, Creds} ->
      case extract_token(Request) of
        {"bearer", Token} -> validate_bearer(Token, Creds);
        {"basic", Token} ->
          %% Allow Basic base64(username:password) as well
          validate_bearer(Token, Creds);
        _ -> false
      end;
    _ -> false
  end;
context_authorize(_, _) -> false.

%% Context rule: Authorize-Via <PathToTool> [HeaderName]
%% Calls tool, passing HeaderName value (default Authorization) to stdin.
%% Tool exit code 0 => authorized.
context_authorize_via([ToolPath], Request) ->
  context_authorize_via([ToolPath, "Authorization"], Request);
context_authorize_via([ToolPath, HeaderName], Request) ->
  Val = parse_http:get_header(HeaderName, Request, ""),
  case run_auth_tool(ToolPath, Val) of
    0 -> true;
    _ -> false
  end;
context_authorize_via(_, _) -> false.

run_auth_tool(Cmd, Input) ->
  Port = open_port({spawn, Cmd}, [binary, exit_status, use_stdio]),
  port_command(Port, Input),
  %% Close stdin so the tool knows no more data is coming
  catch port_close(Port),
  receive
    {Port, exit_status, Status} -> Status
  after 5000 ->
    logging:err("Authorization tool ~s timed out", [Cmd]),
    -1
  end.
