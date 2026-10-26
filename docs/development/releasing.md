# Releasing

Releases are git tags of the form `MAJOR.MINOR-SUFFIX`, for example
`2.0-beta1`. Pushing such a tag runs the test suite, then builds, signs and
publishes the container image under the version and `latest`, and records a
benchmark run.

## 1. Bump the version

The version string lives in four places that must agree:

| File | What to change |
|------|----------------|
| `rebar.config` | `{release, {'MeowMeow', "2.0-beta1"}, [app]}` |
| `src/app.app.src` | `{vsn, "2.0-beta1"}` |
| `src/config.hrl` | `-define(version, "MeowMeow/2.0-beta1" ++ ?GIT_VSN).` |
| `e2e/vsn.txt` | The bare version, used by the `Server` header test. |

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

The `CI` and `Benchmark` workflows match tags with the glob `*.*-*`, so a tag
without a suffix (`2.0`) does **not** trigger a release.

## 4. What the workflow produces

The `CI` workflow runs the unit and end-to-end suites first; the image job only
starts when both passed. It then pushes

- `ghcr.io/0xf104a/meowmeow:<tag>`, `ghcr.io/0xf104a/meowmeow:latest` and
  `ghcr.io/0xf104a/meowmeow:<short sha>`, all the same image, built from the
  `Dockerfile` with `GIT_SHA`, `GIT_REF` and `BUILD_DATE` set, so the `Server`
  header reads `MeowMeow/<version>.<sha>` and the OCI labels name the tag,
  commit and build time (see [Docker](../getting-started/docker.md#image-metadata)).
- A cosign signature for the image digest, made with the workflow's OIDC
  identity. See [Docker](../getting-started/docker.md) for the verification
  command.

The same job publishes a `<short sha>` image for every commit on `master` and
for every pull request from this repository once it is marked ready for
review, so a release image is identical to the one already tested on `master`.

## 5. Documentation

Read the Docs builds `latest` from `master` on every push and builds every
release tag as its own version. Which version the root URL
(<https://meowmeow.readthedocs.io/>) opens is decided by the Read the Docs
project, not by the repository. Read the Docs' built-in `stable` alias skips
pre-releases such as `2.0-beta1`, so the newest tag is made the default with
two automation rules under *Settings → Automation rules* of the project
(one-time setup):

| Match | Version type | Action |
|-------|--------------|--------|
| Custom match `^\d+\.\d+-.+$` | Tag | Activate version |
| Custom match `^\d+\.\d+-.+$` | Tag | Set version as default |

Rules only apply to tags pushed after they were created; set the default
version by hand once under *Settings → Default version* for the current tag.
Benchmark results are committed to `master` after the tag exists, so the
[Benchmark results](../benchmarks.md) page of a tagged version does not
contain its own run; `latest` does.

## 6. Benchmarks

The same tag also triggers the `Benchmark` workflow. It load-tests the
tagged commit, stores the summary as `bench/history/<tag>.json`, regenerates
`docs/benchmarks.md` and commits both to `master` as `github-actions[bot]`
(the commit carries `[skip ci]`). Pull `master` afterwards before you push
anything else, and see [Benchmarking](benchmarking.md) for what the numbers
mean.
