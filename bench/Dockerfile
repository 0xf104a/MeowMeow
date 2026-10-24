# Benchmark image: a production release of the working tree, the benchmark
# configuration under /etc/nya, generated fixtures under /var/www and the
# wrk load generator. Built by bench/run.sh with the repository root as
# context:
#   docker build -t bench.meow.meow -f bench/Dockerfile .

FROM docker.io/library/erlang:alpine AS builder

# GIT_SHA/GIT_DIRTY feed scripts/git_vsn.sh, there is no .git in the context
ARG GIT_SHA=unknown
ARG GIT_DIRTY=

WORKDIR /buildroot
COPY rebar.config rebar.config
COPY scripts scripts
COPY include include
COPY src src
RUN rebar3 as prod release


FROM docker.io/library/erlang:alpine

RUN apk add --no-cache wrk curl

# Server configuration and fixtures
RUN mkdir -p /etc/nya /var/www
COPY config/mimes.conf /etc/nya/mimes.conf
COPY bench/config/meow.conf bench/config/routes.conf /etc/nya/
COPY bench/fixtures.sh /usr/local/bin/meowbench-fixtures
RUN sh /usr/local/bin/meowbench-fixtures /var/www /etc/nya

# Load generator script
COPY bench/wrk/report.lua /opt/bench/report.lua

# Webserver
COPY --from=builder /buildroot/_build/prod/rel/MeowMeow /opt/MeowMeow
COPY bench/entrypoint.sh /usr/local/bin/meowbench
RUN chmod +x /usr/local/bin/meowbench

EXPOSE 8089
ENTRYPOINT ["/usr/local/bin/meowbench"]
CMD ["serve"]
