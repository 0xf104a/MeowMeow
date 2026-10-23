# Writing modules

A module is an Erlang module that declares the `nya_module` behaviour, is
compiled into the release, and is named in `LoadModules`. At startup the
loader calls it to collect the directives it provides.

## The behaviour

`src/nya_module.erl` defines four callbacks:

```erlang
-callback init() -> ok | {error, Reason :: term()}.

-callback get_custom_rules() ->
   [{RuleName :: string(), fun((Args :: [string()], Response) -> Result)}].

-callback get_context_rules() ->
   [{RuleName :: string(), fun((Args :: [string()], Request) -> boolean())}].

-callback terminate(State :: term()) -> ok.
```

`init/0`
:   Called once when the module is loaded, before any rule is registered.
    Create ETS tables or start processes here. Returning anything but `ok`
    stops the server.

`get_custom_rules/0`
:   Directives that act on the response. Each is a pair of the directive
    name as it appears in `routes.conf` and a function of the parsed
    arguments and the response in progress.

`get_context_rules/0`
:   Directives that open a conditional block. Each pair maps a name to a
    function of the arguments and the **request** that returns `true` or
    `false`.

`terminate/1`
:   Reserved for clean shutdown.

Names are matched exactly, so follow the existing `Capitalised-With-Dashes`
style. A name already registered by the core or an earlier module is
silently replaced.

## Arguments

`Args` is the list of tokens after the directive name, as strings. Quoted
tokens arrive as one string without the quotes. A directive written without
arguments passes `true` instead of a list, so match on `[]` and `_` where
appropriate.

```text
Send-File "/var/www/my page.html"   % Args = ["/var/www/my page.html"]
Set-Header X-A "b c"                % Args = ["X-A", "b c"]
```

## Rule functions

A rule receives the `#response{}` record (`src/response.hrl`), whose
`request` field holds the `#request{}` record (`src/request.hrl`). The most
used fields are:

| Record | Field | Content |
|--------|-------|---------|
| `#request{}` | `method` | Binary, for example `<<"GET">>`. |
| | `route` | Request path without the query string. |
| | `header` | Map of header name to value (strings). `parse_http:get_header/3` reads it with a default. |
| | `body` | Body bytes once received (see below). |
| | `http_ver`, `src_addr` | Protocol version and client address. |
| `#response{}` | `code`, `headers`, `body` | What will be sent. `response:update_headers/2`, `response:set_header/3` and `response:override_code/2` modify them. |
| | `upstream` | Pid of the connection's `nya_tcp` process. |
| | `socket` | The raw socket, for rules that stream themselves. |

The rule must return one of:

| Return | Effect |
|--------|--------|
| `{ok, Response}` | Response changed, evaluation continues. |
| `{ready2send, Response}` | Terminal. `handle` adds `Content-Length` and sends `code`, `headers` and `body`. Set `is_ready2send = true` on the record. |
| `{sent, Response}` | Terminal. The rule wrote everything itself through `upstream ! {send, Data}` or the socket. Set `is_sent = true`. |
| `{aborted, Code}` | Terminal. The standard error page for `Code` is sent. |

A crash inside a rule is caught, logged with its stack trace and answered
with `502`.

### Reading a request body

Bodies are not read eagerly. A rule that needs one calls
`handle:handle_body_recv(Response)`, which receives up to `Content-Length`
bytes and returns `{ok, ResponseWithBody}`. The size has already been
checked against `MaxPostSize`.

### Streaming responses

A rule that sends a large or open-ended body should:

1. send `Response#response.upstream ! cancel_tmr` so the keep-alive timer does not close the connection mid-transfer,
2. write the status line and headers with `upstream ! {send, response:response_headers(Headers, Code)}`,
3. write body chunks with `upstream ! {send, Chunk}` (or directly to the socket, as `static_handler:send_file/3` does),
4. send `upstream ! set_tmr` or `upstream ! close`, and return `{sent, Response#response{is_sent = true}}`.

`src/modules/file/static.erl` and `src/modules/mcp/mcp_sse.erl` are complete
examples of both styles.

## Context rule functions

A context function gets `(Args, Request)` and returns a boolean. It must
not have side effects on the response; it only decides whether its block
runs. Return `false` for argument lists you do not understand, as the shipped
modules do, so a typo in `routes.conf` disables the block instead of
crashing the request.

```erlang
context_if_env([Var, Value], _Request) -> os:getenv(Var) == Value;
context_if_env(_, _) -> false.
```

## A complete example

A module adding `If-Method METHOD` (a context rule) and `Redirect URL` (a
terminal rule):

```erlang
-module(redirect).
-behaviour(nya_module).
-include("../../request.hrl").
-include("../../response.hrl").

-export([init/0, terminate/1, get_custom_rules/0, get_context_rules/0]).

init() -> ok.
terminate(_State) -> ok.

get_context_rules() ->
  [{"If-Method", fun context_if_method/2}].

get_custom_rules() ->
  [{"Redirect", fun rule_redirect/2}].

context_if_method([Method], #request{method = Actual}) ->
  list_to_binary(string:uppercase(Method)) =:= Actual;
context_if_method(_, _) -> false.

rule_redirect([Url], Response) ->
  Headers = response:update_headers(Response, #{"Location" => Url,
                                                "Server" => "MeowMeow",
                                                "Date" => util:get_time()}),
  {ready2send, Response#response{is_ready2send = true, code = 302,
                                 body = "", headers = Headers}};
rule_redirect(_, _) ->
  {aborted, 500}.
```

Save it as `src/modules/redirect/redirect.erl`, add `redirect` to
`LoadModules`, and use it:

```text
Route /old-blog*
 If-Method GET
  Redirect https://blog.example.org/
 End
 Abort 405
End
```

## Checklist

- [ ] `-behaviour(nya_module).` is declared; the loader refuses modules without it.
- [ ] The module name (file name) is what users will write in `LoadModules`.
- [ ] Every rule function has a catch-all clause returning `{aborted, Code}` or `false`.
- [ ] Terminal rules set `is_ready2send` or `is_sent` and stop the keep-alive timer while streaming.
- [ ] The directive is documented and, ideally, covered by a case in the integration suite (see [Testing](testing.md)).
