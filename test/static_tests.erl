%%%-------------------------------------------------------------------
%%% @doc EUnit suite for the static module and static_handler
%%%
%%% Files are served straight to the socket with file:sendfile/5, so the
%%% tests hand the code a real loopback socket and read the bytes from
%%% the client end. Headers that go through the upstream process are
%%% received by the test process, which plays upstream.
%%% @end
%%%-------------------------------------------------------------------
-module(static_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include("../src/config.hrl").
-include("../src/request.hrl").
-include("../src/response.hrl").

setup() ->
  test_support:setup_all(#{"LogLevel" => "0", "KeepAlive" => "2000"}),
  {ok, static} = nya_loader:nya_load("static"),
  Dir = test_support:tmp_dir(),
  test_support:write_file(filename:join(Dir, "index.html"), "<h1>index</h1>"),
  test_support:write_file(filename:join(Dir, "file.txt"), "hello"),
  test_support:write_file(filename:join(Dir, "empty.txt"), ""),
  test_support:write_file(filename:join(Dir, "sub/index.html"), "<p>sub</p>"),
  test_support:write_file(filename:join(Dir, "sub/page.txt"), "page"),
  ok = filelib:ensure_path(filename:join(Dir, "dir")),
  test_support:write_file(filename:join(Dir, "big.bin"), crypto:strong_rand_bytes(2 * 1024 * 1024 + 123)),
  Dir.

cleanup(Dir) ->
  test_support:rm_rf(Dir),
  test_support:teardown_all(ok).

static_test_() ->
  {setup, fun setup/0, fun cleanup/1,
   fun(Dir) -> [module_tests(), stat_tests(Dir), filename_tests(Dir),
                docdir_tests(Dir), sendfile_tests(Dir)] end}.

info(Path) -> {ok, I} = file:read_file_info(Path), I.

%% A response whose socket is the server end of a loopback pair; the
%% client end is returned so the test can read what was written.
sock_response(Method, Route, Headers) ->
  test_support:flush(),
  {S, C} = test_support:socket_pair(),
  Req = test_support:request(Method, Route, [{"Host", "h"} | Headers]),
  {(test_support:response_for(Req))#response{socket = S}, C}.

wire(C) -> test_support:recv_all(C, 300).

%%--------------------------------------------------------------------
module_tests() ->
  [{"init/0 returns ok", ?_assertEqual(ok, static:init())},
   {"terminate/1 returns ok", ?_assertEqual(ok, static:terminate(state))},
   {"the module exports the Send-File and DocDir rules (README)",
    ?_assertEqual(["Send-File", "DocDir"], [N || {N, _} <- static:get_custom_rules()])},
   {"the module has no context rules", ?_assertEqual([], static:get_context_rules())}].

%%--------------------------------------------------------------------
stat_tests(Dir) ->
  [{"a missing file (enoent) is no_file", ?_assertEqual({0, no_file}, static_handler:stat_file(enoent))},
   {"{error, enoent} is no_file", ?_assertEqual({0, no_file}, static_handler:stat_file({error, enoent}))},
   {"an unsafe path is no_access", ?_assertEqual({0, no_access}, static_handler:stat_file(unsafe))},
   {"eacces is no_access", ?_assertEqual({0, no_access}, static_handler:stat_file(eacces))},
   {"a readable regular file reports its size",
    ?_assertEqual({5, ok}, static_handler:stat_file({filename:join(Dir, "file.txt"),
                                                     info(filename:join(Dir, "file.txt"))}))},
   {"an empty file is empty_file",
    ?_assertEqual({0, empty_file}, static_handler:stat_file({filename:join(Dir, "empty.txt"),
                                                             info(filename:join(Dir, "empty.txt"))}))},
   {"a directory is no_file",
    ?_assertEqual({0, no_file}, static_handler:stat_file({Dir, info(Dir)}))},
   {"a file without read permission is no_access",
    ?_assertEqual({0, no_access},
                  static_handler:stat_file({"x", (info(filename:join(Dir, "file.txt")))#file_info{access = none}}))},
   {"a write-only file is no_access",
    ?_assertEqual({0, no_access},
                  static_handler:stat_file({"x", (info(filename:join(Dir, "file.txt")))#file_info{access = write}}))}].

%%--------------------------------------------------------------------
filename_tests(Dir) ->
  File = filename:join(Dir, "file.txt"),
  Index = filename:join(Dir, "index.html"),
  SubIndex = filename:join(Dir, "sub/index.html"),
  SubPage = filename:join(Dir, "sub/page.txt"),
  [{"a plain file under DocDir is resolved with its file_info",
    ?_test(begin
             {Path, Info} = static_handler:get_filename(Dir, <<"/file.txt">>),
             ?assertEqual(File, Path),
             ?assertMatch(#file_info{size = 5}, Info)
           end)},
   {"'/' resolves to index.html in DocDir (README: index.html is checked for directories)",
    ?_test(begin
             {index, {Path, Info}} = static_handler:get_filename(Dir, <<"/">>),
             ?assertEqual(Index, Path),
             ?assertMatch(#file_info{type = regular}, Info)
           end)},
   {"a sub-directory with and without trailing slash resolves to its index.html",
    ?_test(begin
             {index, {P1, _}} = static_handler:get_filename(Dir, <<"/sub">>),
             {index, {P2, _}} = static_handler:get_filename(Dir, <<"/sub/">>),
             ?assertEqual(SubIndex, P1),
             ?assertEqual(SubIndex, P2)
           end)},
   {"a file in a sub-directory",
    ?_test(begin
             {Path, Info} = static_handler:get_filename(Dir, <<"/sub/page.txt">>),
             ?assertEqual(SubPage, Path),
             ?assertMatch(#file_info{size = 4}, Info)
           end)},
   {"index.html requested explicitly is served as a plain file",
    ?_test(begin
             {Path, #file_info{}} = static_handler:get_filename(Dir, <<"/index.html">>),
             ?assertEqual(Index, Path)
           end)},
   {"a missing file is enoent", ?_assertEqual(enoent, static_handler:get_filename(Dir, <<"/missing">>))},
   {"a directory without index.html is enoent", ?_assertEqual(enoent, static_handler:get_filename(Dir, <<"/dir">>))},
   {"path traversal with '..' is unsafe", ?_assertEqual(unsafe, static_handler:get_filename(Dir, <<"/../etc/passwd">>))},
   {"path traversal hidden in a sub-directory is unsafe", ?_assertEqual(unsafe, static_handler:get_filename(Dir, <<"/sub/../../x">>))}].

%%--------------------------------------------------------------------
docdir_tests(Dir) ->
  [{"GET of a plain file streams headers and body to the socket with Content-Length and Last-Modified",
    {timeout, 10, ?_test(begin
       {Resp, C} = sock_response("GET", "/file.txt", []),
       ?assertMatch({sent, _}, static_handler:handle_file(Dir, Resp)),
       ?assertEqual([cancel_tmr, set_tmr], test_support:collect(100)),
       {200, H, Body} = test_support:parse_response(wire(C)),
       ?assertEqual("hello", Body),
       ?assertEqual("5", maps:get("Content-Length", H)),
       ?assertEqual(?version, maps:get("Server", H)),
       ?assert(maps:is_key("Date", H)),
       ?assert(maps:is_key("Last-Modified", H)),
       ?assertNot(maps:is_key("Content-Type", H))
     end)}},
   {"GET / serves index.html with Content-Type text/html (mimes.conf cannot match it)",
    {timeout, 10, ?_test(begin
       {Resp, C} = sock_response("GET", "/", []),
       {sent, _} = static_handler:handle_file(Dir, Resp),
       {200, H, Body} = test_support:parse_response(wire(C)),
       ?assertEqual("<h1>index</h1>", Body),
       ?assertEqual("text/html", maps:get("Content-Type", H))
     end)}},
   {"GET /sub/ serves the sub-directory index",
    {timeout, 10, ?_test(begin
       {Resp, C} = sock_response("GET", "/sub/", []),
       {sent, _} = static_handler:handle_file(Dir, Resp),
       ?assertMatch({200, _, "<p>sub</p>"}, test_support:parse_response(wire(C)))
     end)}},
   {"the DocDir rule goes through the rule registry (rules:execute_rule)",
    {timeout, 10, ?_test(begin
       {Resp, C} = sock_response("GET", "/file.txt", []),
       ?assertMatch({sent, _}, rules:execute_rule("DocDir", [Dir], Resp)),
       ?assertMatch({200, _, "hello"}, test_support:parse_response(wire(C)))
     end)}},
   {"a Set-Code override earlier in the chain survives DocDir (status 451 with the file body)",
    {timeout, 10, ?_test(begin
       {Resp0, C} = sock_response("GET", "/file.txt", []),
       {ok, Resp} = rules:execute_rule("Set-Code", ["451"], Resp0),
       {sent, _} = static_handler:handle_file(Dir, Resp),
       ?assertMatch({451, _, "hello"}, test_support:parse_response(wire(C)))
     end)}},
   {"a missing file is answered 404 through the upstream and the connection closed",
    {timeout, 10, ?_test(begin
       {Resp, C} = sock_response("GET", "/missing", []),
       ?assertMatch({sent, _}, static_handler:handle_file(Dir, Resp)),
       Msgs = test_support:collect(100),
       ?assertMatch([{send, _}, close], Msgs),
       [{send, W}] = [M || {send, _} = M <- Msgs],
       ?assertMatch({404, _, _}, test_support:parse_response(W)),
       ?assertEqual(<<>>, wire(C))
     end)}},
   {"path traversal is answered 403",
    {timeout, 10, ?_test(begin
       {Resp, _C} = sock_response("GET", "/../etc/passwd", []),
       {sent, _} = static_handler:handle_file(Dir, Resp),
       [{send, W}, close] = test_support:collect(100),
       ?assertMatch({403, _, _}, test_support:parse_response(W))
     end)}},
   {"an empty file is answered 204 with headers only",
    {timeout, 10, ?_test(begin
       {Resp, _C} = sock_response("GET", "/empty.txt", []),
       {sent, _} = static_handler:handle_file(Dir, Resp),
       [{send, W}, close] = test_support:collect(100),
       ?assertMatch({204, _, ""}, test_support:parse_response(W))
     end)}},
   {"HEAD answers 200 with headers only",
    {timeout, 10, ?_test(begin
       {Resp, C} = sock_response("HEAD", "/file.txt", []),
       {sent, _} = static_handler:handle_file(Dir, Resp),
       [{send, W}] = test_support:collect(100),
       ?assertMatch({200, _, ""}, test_support:parse_response(W)),
       ?assertEqual(<<>>, wire(C))
     end)}},
   {"HEAD carries the same Content-Length / Last-Modified / Date as GET would "
    "(RFC 7231 section 4.3.2: 'The server SHOULD send the same header fields in response to a "
    "HEAD request as it would have sent if the request had been a GET'). "
    "(regression: this used to fail because the HEAD branch of handle_file/2 renders the headers of the Response "
    "received before ResponseHeadered added Content-Length, Date and Last-Modified)",
    {timeout, 10, ?_test(begin
       {Resp, _C} = sock_response("HEAD", "/file.txt", []),
       {sent, _} = static_handler:handle_file(Dir, Resp),
       [{send, W}] = test_support:collect(100),
       {200, H, _} = test_support:parse_response(W),
       ?assertEqual("5", maps:get("Content-Length", H)),
       ?assert(maps:is_key("Last-Modified", H))
     end)}},
   {"a method DocDir cannot serve (POST) is reported as {aborted, 405} so the "
    "client gets an answer. (regression: this used to fail because the catch-all branch builds {aborted, 405} but "
    "discards it (compiler warning 'a term is constructed, but never used') and returns "
    "{sent, Response} although nothing was sent)",
    {timeout, 10, ?_test(begin
       Req = test_support:request("POST", "/file.txt", [{"Host", "h"}, {"Content-Length", "0"}]),
       Resp = test_support:response_for(Req),
       ?assertEqual({aborted, 405}, static_handler:handle_file(Dir, Resp))
     end)}},
   {"a multi-megabyte file is streamed intact (sendfile in 1 MiB chunks)",
    {timeout, 30, ?_test(begin
       {Resp, C} = sock_response("GET", "/big.bin", []),
       {ok, Expected} = file:read_file(filename:join(Dir, "big.bin")),
       Reader = spawn_reader(C, byte_size(Expected)),
       {sent, _} = static_handler:handle_file(Dir, Resp),
       {_, H, Body} = test_support:parse_response(join_reader(Reader)),
       ?assertEqual(integer_to_list(byte_size(Expected)), maps:get("Content-Length", H)),
       ?assertEqual(Expected, list_to_binary(Body))
     end)}}].

%% The server blocks in sendfile until the client drains the socket, so
%% large bodies are read concurrently.
spawn_reader(C, _Min) ->
  Self = self(),
  spawn(fun() -> Self ! {reader, test_support:recv_all(C, 1000)} end).

join_reader(_) -> receive {reader, Bin} -> Bin after 20000 -> erlang:error(reader_timeout) end.

%%--------------------------------------------------------------------
sendfile_tests(Dir) ->
  File = filename:join(Dir, "file.txt"),
  [{"Send-File sends the headers through the upstream and the body to the socket, then closes",
    {timeout, 10, ?_test(begin
       {Resp, C} = sock_response("GET", "/catnip_info", []),
       Result = rules:execute_rule("Send-File", [File], Resp),
       ?assertMatch({sent, #response{is_sent = true}}, Result),
       Msgs = test_support:collect(100),
       ?assertMatch([{send, _}, cancel_tmr, set_tmr, close], Msgs),
       [{send, W}] = [M || {send, _} = M <- Msgs],
       {200, H, ""} = test_support:parse_response(W),
       ?assertEqual("5", maps:get("Content-Length", H)),
       ?assertEqual(?version, maps:get("Server", H)),
       ?assert(maps:is_key("Date", H)),
       ?assertEqual("close", maps:get("Connection", H)),
       ?assertEqual(<<"hello">>, wire(C))
     end)}},
   {"Send-File keeps a keep-alive connection open",
    {timeout, 10, ?_test(begin
       {Resp, C} = sock_response("GET", "/catnip_info", [{"Connection", "keep-alive"}]),
       {sent, _} = rules:execute_rule("Send-File", [File], Resp),
       Msgs = test_support:collect(100),
       ?assertMatch([{send, _}, cancel_tmr, set_tmr], Msgs),
       [{send, W}] = [M || {send, _} = M <- Msgs],
       {200, H, ""} = test_support:parse_response(W),
       ?assertEqual("keep-alive", maps:get("Connection", H)),
       ?assertEqual(<<"hello">>, wire(C))
     end)}},
   {"Send-File after Set-Code 451 answers 451 with the file body (routes.conf /catnip)",
    {timeout, 10, ?_test(begin
       {Resp0, C} = sock_response("GET", "/catnip", []),
       {ok, Resp} = rules:execute_rule("Set-Code", ["451"], Resp0),
       {sent, _} = rules:execute_rule("Send-File", [File], Resp),
       [{send, W} | _] = test_support:collect(100),
       ?assertMatch({451, _, ""}, test_support:parse_response(W)),
       ?assertEqual(<<"hello">>, wire(C))
     end)}},
   {"Send-File of a missing file aborts with 500 (test_rules.rb test_send_file_non_extistent)",
    ?_test(begin
             {Resp, _C} = sock_response("GET", "/get_500", []),
             ?assertEqual({aborted, 500}, rules:execute_rule("Send-File", [filename:join(Dir, "nope")], Resp))
           end)},
   {"Send-File of a directory aborts with 500",
    ?_test(begin
             {Resp, _C} = sock_response("GET", "/x", []),
             ?assertEqual({aborted, 500}, rules:execute_rule("Send-File", [Dir], Resp))
           end)},
   {"Send-File streams a multi-megabyte file intact (test_server.rb test_big_file)",
    {timeout, 30, ?_test(begin
       {Resp, C} = sock_response("GET", "/FatCat", []),
       Big = filename:join(Dir, "big.bin"),
       {ok, Expected} = file:read_file(Big),
       Reader = spawn_reader(C, byte_size(Expected)),
       {sent, _} = rules:execute_rule("Send-File", [Big], Resp),
       [{send, W} | _] = test_support:collect(100),
       {200, H, ""} = test_support:parse_response(W),
       ?assertEqual(integer_to_list(byte_size(Expected)), maps:get("Content-Length", H)),
       ?assertEqual(Expected, join_reader(Reader))
     end)}}].
