# Ghost Web Server
## Abstract
This is simple web server written in plaing erlang.
## Running
Compile all the files using `erl` shell and then you could run it with following command:
```
1> server:run(8888).
```
Tested on `Erlang/OTP 22 [erts-10.5.6] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe] [dtrace]`.

## Using
Put your files into `www` files. They would be served as if `www` was the root dir. File `index.html` would be displayed as defualt if it does exist.

## Code copyrighting
The code copyrightings defined in the begging of the files are not legal advice and purposed for internal use only. 
All code licensed under MIT license.