# Docker

The repository ships a multi-stage `Dockerfile` that builds a production
release on `erlang:alpine` and copies only the release into a fresh runtime
image.

## Prebuilt images

Images are published to the GitHub Container Registry by the `Docker`
workflow whenever a version tag (for example `2.0-alpha10`) is pushed:

```bash
docker pull ghcr.io/0xf104a/meowmeow:2.0-alpha10
```

Every published image is signed with [cosign](https://github.com/sigstore/cosign)
using GitHub's OIDC identity, so the signature can be verified with:

```bash
cosign verify ghcr.io/0xf104a/meowmeow:2.0-alpha10 \
  --certificate-identity-regexp 'https://github.com/0xf104a/MeowMeow/' \
  --certificate-oidc-issuer https://token.actions.githubusercontent.com
```

## Build locally

```bash
docker build -t meowmeow \
  --build-arg GIT_SHA=$(git rev-parse --short HEAD) \
  --build-arg GIT_DIRTY="" .
```

The two build arguments feed the version string reported in the `Server`
header. They default to `unknown`.

## Run

The image listens on port `80` and runs `MeowMeow foreground` through
`/usr/bin/MeowMeow` (a copy of `boot.sh`). Mount your configuration and
content:

```bash
docker run --rm -p 8080:80 \
  -v "$PWD/my-config:/etc/nya:ro" \
  -v "$PWD/site:/var/www:ro" \
  meowmeow
```

where `my-config/` contains `meow.conf`, `routes.conf` and `mimes.conf`.
Inside a container you usually want `ListenHost 0.0.0.0` so the port is
reachable through Docker's port mapping.

!!! warning "Configuration path"
    The release reads its configuration from the paths compiled into
    `src/config.hrl`, which default to `/etc/nya/`. The `Dockerfile` currently
    copies the sample configuration into `/etc/MeowMeow/`, which the binary does
    not read, so always mount or copy your configuration to `/etc/nya/` (or
    change `config.hrl` before building the image).

## Environment-dependent configuration

Because configuration files are static, the `env` module exists to make them
react to the container environment. For example, to return a different
response in a staging deployment:

```text
If-Env DEPLOYMENT staging
 Set-Header X-Robots-Tag noindex
End
```

and run with `-e DEPLOYMENT=staging`. See [env](../modules/env.md).

## Test image

`tests/Dockerfile` builds a second image that installs Ruby, clones the
[SimpleTest](https://github.com/0xf104a/SimpleTest) harness, starts MeowMeow
with the configuration from `tests/config/` and runs the integration suite.
`./test.sh` at the repository root builds and runs it. See
[Testing](../development/testing.md).
