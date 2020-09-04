FROM erlang:23.0-alpine

#Build stuff
COPY src/* ./
RUN erlc *.erl
COPY config/routes.conf routes.conf
COPY boot.erl boot.erl
RUN chmod 755 boot.erl

EXPOSE 8888
ENTRYPOINT escript boot.erl

