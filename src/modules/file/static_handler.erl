%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2026, Anna-Sofia Kasierocka
%%% @doc
%%% Internal procedure related sending files and locating index files
%%% @end
%%%-------------------------------------------------------------------
-module(static_handler).
-author("f104a").
-include("../../request.hrl").
-include("../../response.hrl").
-include("../../config.hrl").
-include_lib("kernel/include/file.hrl").
-export([handle_file/2, send_file/3, stat_file/1]).

send_chunks(Dev, Sock, Sz) ->
  case file:read(Dev, Sz) of
    {ok, Data} ->
      case nya_tcp:tcp_send(Sock, Data) of
        ok -> send_chunks(Dev, Sock, Sz);
        Error -> logging:err("Failed to send chunk: ~p @ handle:send_chunks/3", [Error]),
          {failed, Error}
      end;
    eof -> ok
  end.

send_file(FName, Sock, ChunkSz) ->
  logging:debug("Sending file: ~p", [FName]),
  case file:open(FName, read) of
    {ok, Dev} -> send_chunks(Dev, Sock, ChunkSz);
    Any -> logging:err("Unexpected result while opening the file ~s: ~p", [FName, Any]),
      {failed, Any}
  end.


wrap_fname_stat(FName) ->
  case file:read_file_info(FName) of
    {error, Err} -> Err;
    {ok, FInfo} -> {FName, FInfo}
  end.

get_filename(DocDir, XRoute) ->
  Route = binary:bin_to_list(XRoute, {1, string:length(XRoute) - 1}),
  IndexInDocDir = string:strip(filename:join([Route, "index.html"]), left, $/),
  FNameInDocDir = string:strip(Route),
  logging:debug("~p ~p", [IndexInDocDir, FNameInDocDir]),
  SafeFName = filelib:safe_relative_path(FNameInDocDir, DocDir),
  SafeIName = filelib:safe_relative_path(IndexInDocDir, DocDir),
  FileName = filename:join([DocDir, SafeFName]),
  IndexName = filename:join([DocDir, SafeIName]),
  if (SafeIName == unsafe) or (SafeFName == unsafe) ->
    logging:warn("Request route '~s' is unsafe", [Route]),
    unsafe;
    true ->
      IndexExists = filelib:is_regular(IndexName),
      FNameIsRegular = filelib:is_regular(FileName),
      if IndexExists -> {index, wrap_fname_stat(IndexName)};
        FNameIsRegular -> wrap_fname_stat(FileName);
        true -> enoent
      end
  end.

stat_file({error, enoent}) -> {0, no_file};
stat_file(enoent) -> {0, no_file};
stat_file(no_file) -> {0, no_file};
stat_file(unsafe) -> {0, no_access};
stat_file(eacces) -> {0, no_access};
stat_file({FName, FInfo}) ->
  logging:debug("Stat: ~p, FInfo: ~p", [FName, FInfo]),
  Access = FInfo#file_info.access,
  FSize = FInfo#file_info.size,
  if (Access /= read) and (Access /= read_write) -> {0, no_access};
    FSize == 0 -> {0, empty_file};
    FInfo#file_info.type /= regular -> logging:warn("Attempting to send irregular file: ~s", [FName]),
      {0, no_file};
    true -> {FSize, ok}
  end.

handle_file(Response, Upstream, _FName) when Response#response.request#request.method == "HEAD" ->
  handle:log_response(Response#response.request, 200),
  Upstream ! {send, response:response_headers(Response#response.headers, Response#response.code)};
handle_file(Response, Upstream, FName) ->
  logging:debug("Response=~p", [Response]),
  handle:log_response(Response#response.request, 200),
  Upstream ! {send, response:response_headers(Response#response.headers, Response#response.code)},
  Upstream ! cancel_tmr,
  case send_file(FName, Response#response.socket, ?chunk_size) of
    ok -> pass;
    {failed, _} ->
      logging:warn("Failed to send file, so telling upstream to close connection"),
      Upstream ! close
  end,
  Upstream ! set_tmr,
  ok.

get_filename_or_indexname(DocDir, Route) ->
  case get_filename(DocDir, Route) of
    {index, FStat} ->
      {true, FStat};
    FStat -> {false, FStat}
  end.

%% We need Content-Type for indexes(as mimes.conf unlikely to match those)
maybe_set_html_content_type(true, Headers) ->
  maps:put("Content-Type", "text/html", Headers);
maybe_set_html_content_type(false, Headers) -> Headers.

handle_file(DocDir, Response) ->
  Route = Response#response.request#request.route,
  Request = Response#response.request,
  Method = Response#response.request#request.method,
  Upstream = Response#response.upstream,
  {IsIndex, FStat} = get_filename_or_indexname(DocDir, Route),
  logging:debug("~p ~p ~p", [FStat, DocDir, Route]),
  case stat_file(FStat) of
    {0, no_file} ->
      Upstream ! {send, handle:abort(Method, 404)},
      handle:log_response(Request, 404),
      Upstream ! close;
    {0, no_access} ->
      Upstream ! {send, handle:abort(Method, 403)},
      handle:log_response(Request, 403),
      Upstream ! close;
    {0, empty_file} ->
      Upstream ! {send, handle:abort(<<"HEAD">>, 204)},
      handle:log_response(Request, 204),
      Upstream ! close;
    {ContentSize, ok} ->
      StrTime = util:get_time(),
      ResponseHeadered = response:set_headers(Response,
        maybe_set_html_content_type(IsIndex, #{
          "Content-Length" => integer_to_list(ContentSize),
          "Date" => StrTime,
          "Server" => ?version})),
      {FName, _} = FStat,
      case Method of
        <<"GET">> -> handle_file(ResponseHeadered, Upstream, FName);
        <<"HEAD">> -> Upstream ! {send, response:response_headers(Response#response.headers, Response#response.code)};
        Any -> logging:err("Bad method handling: ~p. Probably a bug @ handle:handle_file/2", [Any]),
          Upstream ! {send, handle:abort(500)}
      end,
      {finished, Response}
  end.
