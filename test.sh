#!/usr/bin/env bash
# Local mirror of the CI pipeline: unit tests first, E2E only if they pass.
# Erlang is not required on the host - both stages run in Docker.
set -u

ERLANG_IMAGE="erlang:28.4.1"

if [[ "${1:-}" != "--skip-eunit" ]]; then
  echo "=> Running EUnit suites in ${ERLANG_IMAGE}"
  docker run --rm -v "$PWD":/src:ro -w /build "${ERLANG_IMAGE}" \
    sh -c 'cp -r /src/. /build/ && rebar3 eunit'
  if [[ $? != 0 ]]; then
    echo "EUnit tests failed - not running E2E tests (use --skip-eunit to force)"
    exit 1
  fi
fi

echo "=> Building E2E test image"
GIT_SHA=$(git rev-parse --short HEAD 2>/dev/null || echo unknown)
GIT_DIRTY=$(git diff --quiet HEAD -- 2>/dev/null || echo dirty)
docker build -t test.meow.meow -f tests/Dockerfile \
  --build-arg "GIT_SHA=$GIT_SHA" --build-arg "GIT_DIRTY=$GIT_DIRTY" .
if [[ $? != 0 ]]; then
   echo "Failed to build test image"
   exit 1
fi
docker run test.meow.meow
