FROM erlang:alpine

# Build
RUN mkdir /buildroot
WORKDIR /buildroot
COPY src src
COPY rebar.config rebar.config
RUN rebar3 as prod release

# Installation
WORKDIR /
RUN mkdir /etc/MeowMeow/
COPY config/routes.conf /etc/MeowMeow/routes.conf
RUN cp -r /buildroot/_build/prod/rel/MeowMeow /MeowMeow
COPY boot.sh /bin/boot.sh
RUN chmod +x /bin/boot.sh

EXPOSE 8888
ENTRYPOINT boot.sh

