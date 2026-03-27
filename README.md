# MeowMeow
[![Erlang CI](https://github.com/Andrewerr/MeowMeow/actions/workflows/erlang.yml/badge.svg)](https://github.com/Andrewerr/MeowMeow/actions/workflows/erlang.yml)
[![Publish Docker image](https://github.com/Andrewerr/MeowMeow/actions/workflows/docker.yml/badge.svg)](https://github.com/Andrewerr/MeowMeow/actions/workflows/docker.yml)
[![Tests](https://github.com/Andrewerr/MeowMeow/actions/workflows/test.yml/badge.svg)](https://github.com/Andrewerr/MeowMeow/actions/workflows/test.yml)

# Erlang version
Lastly, tested on Erlang/OTP 28, and it is the recommended version now.
# Running
## Configuration
First, you need to copy configurations:
```bash
cp -r configs/ /etc/MeowMeow/ # You may need sudot
```

Alternatively, you may edit [src/config.hrl](src/config.hrl):
```erlang
%%...
%% Replace /etc/MeowMeow/ with your desired path to configs
-define(accessfile, "/etc/MeowMeow/routes.conf").
-define(configfile, "/etc/MeowMeow/meow.conf").
%%...
```
## Debug mode
The best way to debug the program is just to run it with rebar3:
```bash
rebar3 shell
```
## Production mode 
Compile using rebar3:
```
$ rebar3 as prod release
```
Then you need to create configs in `/etc/MeowMeow/`. After this you can run server:
```
$ ./_build/prod/rel/MeowMeow/bin/MeowMeow <desired mode of running>
```
If you need help on modes of running just execute script with no arguments to get help. 

# Configuring

**IMPORTANT NOTICE:** In current version syntax errors in config are **NOT** checked, so misconfiguration may lead to fatal errors.

## Server
Server configuration is stored in `/etc/MeowMeow/meow.conf`. The syntax is as follows:
```
Directive1 Args
Directive2 Args
```
Current version support following directives:
* `LogLevel <<LEVEL>>` set logging level from 0 to 4(0 -- log nothing,4 -- log everything)
* `KeepAlive <<MS>>` default connection keep-alive time in milliseconds
* `ListenPort <<PORT>>` port where to listen for connections
* `ListenHost <<HOSTNAME/IP>>` hostname to listen on
* `DocDir <<DIRECTORY>>` directory with files to serve

## Routes
To configure routes you need to edit `/etc/MeowMeow/routes.conf`. The syntax is as follows:
```
Route <wildcard pattern> 
 Directive1 Args 
 Directive2 Args
 Host <wildcard pattern>
  Directive3 Args
 End
End
```
`Route` defines pattern of request paths for which directives would be applied. Additionally directives can be applied by `Host` header(as in an example above). Directives are applied in order as they added in the config file.
The directives currently supported by server:
* `Abort <<CODE>>` - stop processing request and send HTTP/1.1 status code `<<CODE>>` to client
* `No-Content` - sends `HTTP/1.1 204 No Content` to client
* `Disallow` - sends `HTTP/1.1 403 Forbidden` to client
* `Set-Header <<HEADER>> <<VALUE>>` - sets response header `<<HEADER>>` to `<<VALUE>>`
* `Set-Code <<CODE>>` sets status code for a response.

> [!WARNING]
> There are, so called, "terminal" directives.
> They stop further processing of the request and send response to client.
> In this section those are `Abort`, `No-Content` and `Disallow`. 
> Modules must specifically tell which directives are terminal.

### Handling the route a few times
Sometimes, you cannot apply the terminal directive straight ahead. For example, 
you may want to set a header, then process mimes, and if the host is localhost, do completely something else, but
keeping the same header. In such a case you may split rules and apply them twice as in example below:

```
# First set header which will be applied to all routes
Route *
  Set-Header X-Powered-By "Pusheen The Cat"
End

# Now apply mimes
Include "/etc/MeowMeow/mimes.conf"

# Now apply localhost rule, so if Host header is localhost,
# we will send localcat.html
Host localhost
  Set-Header Content-Type "text/html"
  Send-File "/var/www/html/localcat.html"
End

# Finally, apply default rule: send respective file from /var/www/html
Route *
  DocDir /var/www/html
End

# It is impossible to reach this route,
# since DocDir already would apply to it and
# send off request to the client.
Route /never_reachable
  Abort 418
End
```

You can see an example of routing rules configuration [here](config/routes.conf) or in [tests](tests/config/routes.conf)

## Modules
From scratch MeowMeow would not serve anything.
Default configuration, though loads module `static` which serves static files from `/var/www/` directory by default.
If you need any additional modules or do not want to static you should edit `/etc/MeowMeow/modules.conf` file:
```
LoadModules <module1>,<module2>...
```
### static
Static module is intended to serve static files.
#### `DocDir`
Automatically serves any file from a given directory. 
For example, if you would like to server from traditional `/var/www/html`, you may write:
```
DocDir /var/www/html
```
> [!INFO]
> This is a terminal rule. It will send off the response and stop further processing.

#### `Send-File`
Sends file from a given path. Accepts single argument, which is a path to file.
```
Send-File /opt/meow/nya.html
```

> [!INFO]
> This is a terminal rule. It will send off the response and stop further processing.