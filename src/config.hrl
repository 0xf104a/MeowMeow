%%%-------------------------------------------------------------------
%%% @author p01ar
%%% @copyright (C) 2020, Polar Group
%%% @doc
%%%  Build config for MeowMeow Webserver
%%% @end
%%% Created : 26. авг. 2020 19:50
%%%-------------------------------------------------------------------
%%DONE:Config file
-author("p01ar").
-record(sockaddr_in4, {family = inet, port = 8888, addr = {0, 0, 0, 0}}).
-define(CHUNK_SIZE, 2048).
-define(version, "MeowMeow/1.02-prebeta-r13").
-define(accessfile, "/etc/MeowMeow/routes.conf").
-define(max_request_length, 10000).
-define(mime_types_file, "/etc/MeowMeow/mime.types").
-define(docdir, configuration:get("DocDir",string)).
-define(chunk_size, 1400).
-define(timeout, list_to_integer(configuration:get("KeepAlive"))).
-define(configfile, "/etc/MeowMeow/meow.conf").
-define(defconf, #{"DocDir"=>"/var/www/",
                   "LogLevel" => "0", 
                   "ListenHost" => "127.0.0.1", 
                   "KeepAlive"=> "10000", 
                   "ListenPort"=>"80", 
                   "AllowLegacyHttp"=>"No",
                   "MaxPostSize"=>"10240"
                  }). %% Default configuration
