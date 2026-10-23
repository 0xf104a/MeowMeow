# Releasing

Releases are git tags of the form `MAJOR.MINOR-SUFFIX`, for example
`2.0-alpha10`. Pushing such a tag builds, signs and publishes the container
image.

## 1. Bump the version

The version string lives in four places that must agree:

| File | What to change |
|------|----------------|
| `rebar.config` | `{release, {'MeowMeow', "2.0-alpha10"}, [app]}` |
| `src/app.app.src` | `{vsn, "2.0-alpha10"}` |
| `src/config.hrl` | `-define(version, "MeowMeow/2.0-alpha10" ++ ?GIT_VSN).` |
| `tests/vsn.txt` | The bare version, used by the `Server` header test. |

The documentation site reads nothing from these files, so no change is needed
there.

## 2. Verify

```bash
rebar3 compile
./test.sh
```

## 3. Tag and push

```bash
git tag 2.0-alpha11
git push origin master 2.0-alpha11
```

The `Docker` workflow matches tags with the glob `*.*-*`, so a tag without a
suffix (`2.0`) does **not** trigger it.

## 4. What the workflow produces

- `ghcr.io/0xf104a/meowmeow:<tag>`, built from the `Dockerfile` with `GIT_SHA` set to the short commit hash, so the `Server` header of the image reads `MeowMeow/<version>.<sha>`.
- A cosign signature stored next to the image, made with the workflow's OIDC identity. See [Docker](../getting-started/docker.md) for the verification command.

## 5. Documentation

Read the Docs builds the `latest` version from `master` on every push and
can be configured to build tagged versions too. Enable the tag under
*Versions* in the Read the Docs project after pushing it.

## 6. Benchmarks

The same tag also triggers the `Benchmark` workflow. It load-tests the
tagged commit, stores the summary as `bench/history/<tag>.json`, regenerates
`docs/benchmarks.md` and commits both to `master` as `github-actions[bot]`
(the commit carries `[skip ci]`). Pull `master` afterwards before you push
anything else, and see [Benchmarking](benchmarking.md) for what the numbers
mean. The tagged documentation version itself does not contain its own
results; only `latest` does.
