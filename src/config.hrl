%%%-------------------------------------------------------------------
%%% @author f104a
%%% @copyright (C) 2020, 2026, Anna-Sofia Kasierocka
%%% @doc
%%%  Buildtime config for MeowMeow Webserver
%%% @end
%%%-------------------------------------------------------------------
%%DONE:Config file
-author("f104a").
-include("git_vsn.hrl").
-record(sockaddr_in4, {family = inet, port = 8888, addr = {0, 0, 0, 0}}).
-define(CHUNK_SIZE, 2048).
-define(version, "MeowMeow/2.0-alpha9" ++ ?GIT_VSN).
-define(accessfile, "/etc/MeowMeow/routes.conf").
-define(configfile, "/etc/MeowMeow/meow.conf").
-define(max_request_length, 10000).
-define(chunk_size, 65536).
-define(timeout, list_to_integer(configuration:get("KeepAlive"))).
-define(defconf, #{"DocDir"=>"/var/www/",
                   "LogLevel" => "0", 
                   "ListenHost" => "127.0.0.1", 
                   "KeepAlive"=> "10000", 
                   "ListenPort"=>"80", 
                   "AllowLegacyHttp"=>"No",
                   "MaxPostSize"=>"10240"
                  }). %% Default configuration
%% Base headers
-define(base_headers, #{"Server" => ?version}).
