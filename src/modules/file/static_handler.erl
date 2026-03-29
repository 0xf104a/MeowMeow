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

send_file(Socket, Path, Size) ->
  send_file(Socket, Path, Size, <<>>).

send_file(Socket, Path, Size, Headers) ->
  socket:setopt(Socket, tcp, cork, true),
  case Headers of
    <<>> -> ok;
    _ -> socket:send(Socket, Headers)
  end,
  {ok, Fd} = file:open(Path, [read, raw, binary]),
  try
    file:sendfile(Fd, Socket, 0, Size, [{chunk_size, 1048576}])
  after
    file:close(Fd)
  end,
  socket:setopt(Socket, tcp, cork, false).

wrap_fname_stat(FName) ->
  case file:read_file_info(FName) of
    {error, Err} -> Err;
    {ok, FInfo} -> {FName, FInfo}
  end.

get_filename(DocDir, XRoute) ->
  Route = binary:bin_to_list(XRoute, {1, string:length(XRoute) - 1}),
  IndexInDocDir = string:strip(filename:join([Route, "index.html"]), left, $/),
  FNameInDocDir = string:strip(Route),
  %%logging:debug("~p ~p", [IndexInDocDir, FNameInDocDir]),
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
  %%logging:debug("Stat: ~p, FInfo: ~p", [FName, FInfo]),
  Access = FInfo#file_info.access,
  FSize = FInfo#file_info.size,
  if (Access /= read) and (Access /= read_write) -> {0, no_access};
    FSize == 0 -> {0, empty_file};
    FInfo#file_info.type /= regular -> logging:warn("Attempting to send irregular file: ~s", [FName]),
      {0, no_file};
    true -> {FSize, ok}
  end.

handle_file(Response, Upstream, _, _) when Response#response.request#request.method == "HEAD" ->
  handle:log_response(Response#response.request, 200),
  Upstream ! {send, self(),
    list_to_binary(response:response_headers(Response#response.headers, Response#response.code))},
  receive
    sent -> ok;
    _ -> error
  end;

handle_file(Response, Upstream, FName, FInfo) ->
  %%logging:debug("Response=~p", [Response]),
  handle:log_response(Response#response.request, 200),
  Headers = response:response_headers(Response#response.headers, Response#response.code),
  Upstream ! cancel_tmr,
  case send_file(Response#response.socket, FName, FInfo#file_info.size, Headers) of
    ok -> pass;
    {ok, _} -> pass;
    {failed, Reason} ->
      logging:err("File sending failed: ~p", [Reason]),
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
      {FName, FInfo} = FStat,
      ResponseHeadered = response:set_headers(Response,
        maybe_set_html_content_type(IsIndex, #{
          "Content-Length" => integer_to_list(ContentSize),
          "Date" => StrTime,
          "Last-Modified" => util:format_file_time(FInfo#file_info.mtime),
          "Server" => ?version})),
      case Method of
        <<"GET">> -> handle_file(ResponseHeadered, Upstream, FName, FInfo);
        <<"HEAD">> -> Upstream ! {send, response:response_headers(Response#response.headers, Response#response.code)};
        Any -> logging:err("Bad method handling: ~p. Probably a bug @ handle:handle_file/2", [Any]),
          Upstream ! {send, handle:abort(500)}
      end,
      {sent, Response}
  end.
