# MeowMeow webserver (formerly Ghost WebServer)
## Abstract
This is simple web server written in plaing erlang.
## Erlang version
`Erlang/OTP 23` is required to run this sever.
## Running
### Debug mode
The best way to debug the program is just to do as following:
```
cd src/
erlc *.erl && erl; rm -rf ./*.beam # This will rebuild everything from scratch, so there would be no problems with cached files
```
Before using this approach don't forget to create configuration files:
```
cp -r configs/ /etc/MeowMeow/ # Porbably you will need root(i.e. sudo)
```
### Production mode 
Compile using rebar3:
```
rebar3 as prod release
```
Then you need to create configs in `/etc/MeowMeow/`. After this you can run server:
```
 ./_build/prod/rel/MeowMeow/bin/MeowMeow <desired mode of running>
```
If you need help on modes of running just execute script with no arguments to get help. 

## Using
Put your files in `/var/www/` directory they will be served statically. No gateway interface support present yet.

## Configuring

**IMPORTANT NOTICE:** In current version syntax errors in config are **NOT** checked, so misconfiguration may lead to fatal errors.

### Server
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
### Routes
To configure routes you need to edit `/etc/MeowMeow/routes.conf`. The syntax is as follows:
```
Route <wildcard pattern> 
 Directive1 Args 
 Directive2 Args
End
```
`Route` defines pattern for which directives would be applied. Directives are applied in order as they added in the config file.
The directives currently supported by server:
* `Abort <<CODE>>` - stop processing request and send HTTP/1.1 status code `<<CODE>>` to client
* `No-Content` - sends `HTTP/1.1 204 No Content` to client
* `Disallow` - sends `HTTP/1.1 403 Forbidden` to client
* `Set-Header <<HEADER>> <<VALUE>>` - sets response header `<<HEADER>>` to `<<VALUE>>`
* `ExecFCGI <<FILE>> <<FCGI_HOST>> <<FCGI_PORT>> <<FCGI_TIMEOUT>>` - asks FastCGI running on `<<FCGI_HOST>>:<<FCGI_PORT>>` to execute `<<FILE>>` with timeout of `<<FCGI_TIMEOUT>>` ms

## Credits 
* erl_fastcgi - Copyright 2017, Marcelo Gornstein <marcelog@gmail.com> (Apache-2.0 license).<br> Changes introduced(file: `src/erl_fastcgi.erl`):
  * Added logging integrated with MeowMeow webserver
  * Added handling of errors when FastCGI server is down
## Code copyrighting
The code copyrightings defined in some of the files in `src/` directory are not legal advice and purposed for internal use only. 
All code, except mentioned in **Credits** section, is licensed under MIT license(See [LICENSE](LICENSE) for more information)

