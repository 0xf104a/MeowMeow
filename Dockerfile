FROM erlang:23.0-alpine

#Build stuff
COPY src/* ./
RUN erlc *.erl
COPY config/routes.conf routes.conf
COPY boot.escript boot.escript
RUN chmod 755 boot.escript

EXPOSE 8888
ENTRYPOINT escript boot.escript

