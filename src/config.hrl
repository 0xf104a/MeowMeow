%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%  Build config for MeowMeow Webserver
%%% @end
%%% Created : 26. авг. 2020 19:50
%%%-------------------------------------------------------------------
-author("p01ar").
-record(sockaddr_in4, {family = inet, port = 8888, addr = {0, 0, 0, 0}}).
-define(CHUNK_SIZE, 2048).
-define(TIMEOUT, 1000).
-define(version, "MeowMeow/1.02-alpha-40920").
-define(accessfile, "routes.conf").
-define(max_request_length, 10000).
-define(mime_types_file, "mime.types").