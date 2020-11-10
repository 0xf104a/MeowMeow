-module(response).
-export([response/3, get_desc/1, response_headers/2, do_response_headers/1, response_error/1, set_header/3,
  set_headers/2, update_headers/2]).
-include("config.hrl").
-include("response.hrl").

get_desc(Code) ->
  Responses = #{
    "100" => "Continue",
    "101" => "Switching Protocols",
    "102" => "Processing",
    "200" => "OK",
    "201" => "Created",
    "202" => "Accepted",
    "203" => "Non-Authoritative Information",
    "204" => "No Content",
    "205" => "Reset Content",
    "206" => "Partial Content",
    "207" => "Multi-Status",
    "208" => "Already Reported",
    "226" => "IM Used",
    "300" => "Multiple Choices",
    "301" => "Moved Permanently",
    "302" => "Found",
    "303" => "See Other",
    "304" => "Not Modified",
    "305" => "Use Proxy",
    "307" => "Temporary Redirect",
    "308" => "Permanent Redirect",
    "400" => "Bad Request",
    "401" => "Unauthorized",
    "402" => "Payment Required",
    "403" => "Forbidden",
    "404" => "Not Found",
    "405" => "Method Not Allowed",
    "406" => "Not Acceptable",
    "407" => "Proxy Authentication Required",
    "408" => "Request Timeout",
    "409" => "Conflict",
    "410" => "Gone",
    "411" => "Length Required",
    "412" => "Precondition Failed",
    "413" => "Request Entity Too Large",
    "414" => "Request-URI Too Long",
    "415" => "Unsupported Media Type",
    "416" => "Requested Range Not Satisfiable",
    "417" => "Expectation Failed",
    "418" => "I'm a teapot",
    "422" => "Unprocessable Entity",
    "423" => "Locked",
    "424" => "Failed Dependency",
    "426" => "Upgrade Required",
    "428" => "Precondition Required",
    "429" => "Too Many Requests",
    "431" => "Request Header Fields Too Large",
    "500" => "Internal Server Error",
    "501" => "Not Implemented",
    "502" => "Bad Gateway",
    "503" => "Service Unavailable",
    "504" => "Gateway Timeout",
    "505" => "HTTP Version Not Supported",
    "506" => "Variant Also Negotiates",
    "507" => "Insufficient Storage",
    "508" => "Loop Detected",
    "510" => "Not Extended",
    "511" => "Network Authentication Required"
  },
  maps:get(Code, Responses).

response2str(PList, Index, CurStr) ->
  if Index > length(PList) -> CurStr;
    true -> Pair = lists:nth(Index, PList),
      {Key, Val} = Pair,
      response2str(PList, Index + 1, CurStr ++ Key ++ ": " ++ Val ++ "\r\n")
  end.

response(Params, StatusCode, Body) ->
  PList = maps:to_list(Params),
  Code = integer_to_list(StatusCode),
  Header = "HTTP/1.1 " ++ Code ++ " " ++ get_desc(Code) ++ "\r\n",
  response2str(PList, 1, Header) ++ "\r\n" ++ Body.

response_headers(Params, StatusCode) ->
  PList = maps:to_list(Params),
  Code = integer_to_list(StatusCode),
  Header = "HTTP/1.1 " ++ Code ++ " " ++ get_desc(Code) ++ "\r\n",
  response2str(PList, 1, Header) ++ "\r\n".

do_response_headers(Resp) ->
  response_headers(Resp#response.headers, Resp#response.code).

response_error(Code) ->
  StrTime = util:get_time(),
  #response{code = Code,
    headers =
    #{"Server" => ?version,
      "Date" => StrTime,
      "Content-Type" => "text/html"}
  }.

set_header(Response, Header, Value) ->
  Response#response{headers = maps:merge(Response#response.headers, #{Header => Value})}.

set_headers(Response, Headers) ->
  Response#response{headers = maps:merge(Response#response.headers, Headers)}.

update_headers(Response, Headers) ->
  maps:merge(Response#response.headers, Headers).


   
   
