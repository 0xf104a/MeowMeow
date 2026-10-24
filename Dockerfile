# Production image: a self-contained release on erlang:alpine with the sample
# configuration in /etc/nya and /var/www as the document root.
#
# Build from the repository root. The build arguments carry the commit the
# image was built from; CI fills them in, and locally:
#
#   docker build -t meowmeow \
#     --build-arg GIT_SHA=$(git rev-parse --short HEAD) \
#     --build-arg GIT_REF=$(git rev-parse --abbrev-ref HEAD) \
#     --build-arg BUILD_DATE=$(date -u +%Y-%m-%dT%H:%M:%SZ) .
#
# They end up in three places: the OCI labels (docker inspect), the
# MEOWMEOW_* environment of the container, and the Server header, which the
# rebar3 pre-compile hook (scripts/git_vsn.sh) builds from GIT_SHA/GIT_DIRTY
# because the build context has no .git directory.
ARG GIT_SHA=unknown
ARG GIT_DIRTY=
ARG GIT_REF=unknown
ARG BUILD_DATE=unknown

FROM erlang:alpine AS builder
ARG GIT_SHA
ARG GIT_DIRTY

WORKDIR /buildroot
COPY rebar.config rebar.config
COPY scripts scripts
COPY include include
COPY src src
RUN rebar3 as prod release


FROM erlang:alpine
ARG GIT_SHA
ARG GIT_DIRTY
ARG GIT_REF
ARG BUILD_DATE

LABEL org.opencontainers.image.title="MeowMeow" \
      org.opencontainers.image.description="A tiny, modular HTTP/1.1 web server written in Erlang/OTP with an MCP gateway" \
      org.opencontainers.image.source="https://github.com/0xf104a/MeowMeow" \
      org.opencontainers.image.url="https://meowmeow.readthedocs.io/" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.revision="${GIT_SHA}" \
      org.opencontainers.image.version="${GIT_REF}" \
      org.opencontainers.image.created="${BUILD_DATE}"

ENV MEOWMEOW_COMMIT="${GIT_SHA}${GIT_DIRTY:+.dirty}" \
    MEOWMEOW_REF="${GIT_REF}" \
    MEOWMEOW_BUILD_DATE="${BUILD_DATE}"

# Sample configuration, listening on all interfaces so the port mapping works.
# Mount your own files over /etc/nya to replace it.
COPY config/routes.conf config/mimes.conf config/meow.conf /etc/nya/
RUN sed -i 's/^ListenHost .*/ListenHost 0.0.0.0/' /etc/nya/meow.conf

COPY --from=builder /buildroot/_build/prod/rel/MeowMeow /opt/MeowMeow
COPY boot.sh /usr/bin/MeowMeow
RUN chmod +x /usr/bin/MeowMeow
COPY www /var/www

EXPOSE 80
ENTRYPOINT ["/usr/bin/MeowMeow"]
