# MeowMeow webserver (formerly Ghost WebServer)
## Abstract
This is simple web server written in plaing erlang.
## Running
### Debug mode
The best way to debug the program is just to do as following:
```
cd src/
erlc *.erl && erl && rm -rf ./*.beam # This will rebuild everything from scratch, so there would be no problems with cached files
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

## Using
Put your files into `www` files. They would be served as if `www` was the root dir. File `index.html` would be displayed as defualt if it does exist.

## Code copyrighting
The code copyrightings defined in  the files in `src/` directory are not legal advice and purposed for internal use only. 
All code licensed under MIT license(See [LICENSE](LICENSE) for more information)
