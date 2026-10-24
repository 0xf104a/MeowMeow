# Docker

The repository ships a multi-stage `Dockerfile` that builds a production
release on `erlang:alpine` and copies only the release, the sample
configuration and `www/` into a fresh runtime image.

## Prebuilt images

Images are published to the GitHub Container Registry by the `CI` workflow,
and only after the unit and end-to-end suites have passed on the same commit:

| Tag | Published for |
|-----|---------------|
| `2.0-beta1` (the git tag) | every release tag `*.*-*` |
| `latest` | moved to every release tag |
| `b51811c` (short commit hash) | every commit on `master`, every release tag, and every pull request from this repository once it is ready for review (drafts are tested but not published) |

```bash
docker pull ghcr.io/0xf104a/meowmeow:latest        # newest release
docker pull ghcr.io/0xf104a/meowmeow:2.0-beta1   # a specific release
docker pull ghcr.io/0xf104a/meowmeow:b51811c       # a specific commit
```

Every published image is signed with [cosign](https://github.com/sigstore/cosign)
using GitHub's OIDC identity, so the signature can be verified with:

```bash
cosign verify ghcr.io/0xf104a/meowmeow:latest \
  --certificate-identity-regexp 'https://github.com/0xf104a/MeowMeow/' \
  --certificate-oidc-issuer https://token.actions.githubusercontent.com
```

## Image metadata

The commit an image was built from is recorded in three places:

- **OCI labels**, shown by `docker inspect`: `org.opencontainers.image.revision`
  (short commit hash), `org.opencontainers.image.version` (the git tag or
  branch) and `org.opencontainers.image.created` (build time, UTC), next to
  the static `source`, `url`, `title` and `licenses` labels.
- **Environment** of the container: `MEOWMEOW_COMMIT`, `MEOWMEOW_REF` and
  `MEOWMEOW_BUILD_DATE`. The entry point prints them together with the release
  version on start.
- The **`Server` header** and the startup banner: `MeowMeow/<version>.<commit>`,
  with a `.dirty` suffix when the image was built from a tree with uncommitted
  changes.

```bash
docker inspect ghcr.io/0xf104a/meowmeow:latest \
  --format '{{index .Config.Labels "org.opencontainers.image.revision"}} {{index .Config.Labels "org.opencontainers.image.version"}} {{index .Config.Labels "org.opencontainers.image.created"}}'
```

## Build locally

```bash
docker build -t meowmeow \
  --build-arg GIT_SHA=$(git rev-parse --short HEAD) \
  --build-arg GIT_REF=$(git rev-parse --abbrev-ref HEAD) \
  --build-arg BUILD_DATE=$(date -u +%Y-%m-%dT%H:%M:%SZ) .
```

The build context contains no `.git` directory, so the build arguments are the
only source of the metadata above; `GIT_DIRTY=dirty` marks a build from a tree
with uncommitted changes. Without them the labels read `unknown` and the
`Server` header `MeowMeow/<version>.unknown`.

## Run

The image listens on port `80` on all interfaces and runs `MeowMeow foreground`
through `/usr/bin/MeowMeow` (a copy of `boot.sh`). It ships the sample
configuration from `config/` in `/etc/nya/` and `www/` as the document root;
mount your own over them:

```bash
docker run --rm -p 8080:80 \
  -v "$PWD/my-config:/etc/nya:ro" \
  -v "$PWD/site:/var/www:ro" \
  meowmeow
```

where `my-config/` contains `meow.conf`, `routes.conf` and `mimes.conf`. Keep
`ListenHost 0.0.0.0` in your `meow.conf` so the port is reachable through
Docker's port mapping; the sample configuration inside the image is patched to
that value at build time.

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
