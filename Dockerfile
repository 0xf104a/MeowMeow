FROM erlang:alpine AS builder

# Build
RUN mkdir /buildroot
WORKDIR /buildroot
COPY src src
COPY include include
COPY rebar.config rebar.config
RUN rebar3 as prod release



FROM erlang:alpine

# Installation
## Configs
WORKDIR /
RUN mkdir /etc/MeowMeow/
COPY config/routes.conf /etc/MeowMeow/routes.conf
COPY config/mimes.conf /etc/MeowMeow/mimes.conf
COPY config/meow.conf /etc/MeowMeow/meow.conf

## Webserver
COPY --from=builder /buildroot/_build/prod/rel/MeowMeow /opt/MeowMeow
COPY boot.sh /usr/bin/MeowMeow
RUN chmod +x /usr/bin/MeowMeow
COPY www /var/www

# Tell docker how to run app
EXPOSE 80
ENTRYPOINT ["/usr/bin/MeowMeow"] 

