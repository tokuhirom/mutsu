---
name: cut-release
description: Cut a mutsu release — choose the version by semver judgment, land a version-bump pull request, push the `vX.Y.Z` tag on its merge commit, and verify that the tag build publishes all four tarballs, the npm package, and the GitHub Release. Use when asked to release, cut a version, tag a release, bump the version, or publish mutsu.
metadata:
  short-description: Release mutsu with a bump PR and a hand-pushed tag
---

# Cutting a release

A release needs two steps and no GitHub App:

1. A **version-bump pull request**, merged like any other PR after CI.
2. A **`vX.Y.Z` tag** on its merge commit, pushed by the maintainer's account.

The tag push fires `release.yml`. tagpr was removed on 2026-07-25, and the `tag-release.yml` App
workflow on 2026-10-03. There is no release PR bot, no `CHANGELOG.md`, and no `minor`/`major`
label to apply on ordinary PRs.

**Push a `v*` tag only here, and only when the user asked for a release.** A tag publishes
binaries, an npm package and a Docker image. Pushing one is never a side effect of other work.

The `gh` commands below are written for the local dev box. A remote container has no `gh`.
Translate them with the mapping table in
[docs/agent-environments.md](../../../docs/agent-environments.md): `create_pull_request` /
`enable_pr_auto_merge` for the bump PR, `actions_list` / `get_job_logs` to follow the run,
`get_release_by_tag` to verify the Release. Every `git` command is the same in both environments.

## 1. Pick the version by hand

There is no label-driven *version* automation. Use semver judgment over what actually merged
since the last tag:

```bash
git fetch origin main --tags
git log --oneline "$(git describe --tags --abbrev=0 origin/main)"..origin/main
```

- **patch:** fixes, roast progress, docs
- **minor:** a new user-visible feature
- **major:** a breaking change

## 2. Land the version-bump pull request

```bash
V=0.25.0                                   # no `v` prefix
git checkout -b "release/v$V" origin/main
sed -i -E "0,/^version = \".*\"$/ s//version = \"$V\"/" Cargo.toml
cargo update -p mutsu --offline            # syncs only the root `mutsu` entry of Cargo.lock
git diff --stat                            # exactly Cargo.toml and Cargo.lock, one line each
git commit -am "Release v$V"
git push -u origin "release/v$V"
gh pr create --title "chore(release): v$V" --body "Bump the version to $V for the v$V release."
gh pr merge --auto --merge
```

The bump touches only the root package. `crates/mutsu-lsp` is versioned on its own
(docs/language-server.md). The PR runs full CI, since `Cargo.toml` is a build input, so the
commit you are about to tag has passed CI. Wait for GitHub to report the PR `MERGED`.

## 3. Push the tag on the merge commit

```bash
git fetch origin main
M=$(git rev-parse origin/main)             # the merge commit of the bump PR...
git show "$M:Cargo.toml" | grep -m1 '^version'   # ...whose Cargo.toml says $V
git tag -a "v$V" "$M" -m "Release v$V"
git push origin "v$V"
```

If other PRs merged right after the bump, tag the bump PR's own merge commit, not whatever
`main` is now. Any commit on `main` whose `Cargo.toml` says `$V` is valid.

The push must come from the maintainer's account: a session's `git push`, or the maintainer at
a terminal. A tag pushed with a workflow's `GITHUB_TOKEN` does not start `release.yml`. The `v*`
tag ruleset lets only the maintainer create these tags.

## 4. What the tag sets in motion

`release.yml`:

| Job | What it does |
| --- | --- |
| `verify-tag` | Refuses a tag whose version differs from the tagged commit's `Cargo.toml`, or whose commit is not on `main`. Nothing builds or publishes past it. |
| `build` | Builds `mutsu` + `mzef` for all four targets (Linux x64/arm64, macOS x64/arm64) and packages `bin/` + `share/mutsu/zef` tarballs. **All four are required**; none is `continue-on-error`. |
| `batteries` | Release gate: every bundled library's upstream test suite must still pass at its recorded baseline against the shipped library and this mutsu (`scripts/battery-testsuite.sh`). |
| `npm` | Builds the browser/WASM package and publishes `@tokuhirom/mutsu` via OIDC trusted publishing, in the `release` environment. That environment has **required reviewers**: the job waits until the maintainer approves the deployment. npm records provenance automatically. **Do not add a long-lived npm token.** |
| `release` | Downloads the artifacts, writes `mutsu-vX.Y.Z-SHA256SUMS.txt`, attests build provenance for every tarball, and creates the GitHub Release with `generate_release_notes: true`. |

`docker.yml` also fires on the tag and pushes `ghcr.io/tokuhirom/mutsu:X.Y.Z` and `:latest`.

`mutsu --version` reports `env!("CARGO_PKG_VERSION")`, so the version written in step 2 is the
version that ships.

## 5. Hand the npm approval to the maintainer

The `npm` job pauses at "Waiting for review" until someone approves the `release` deployment, and
the `release` job (the GitHub Release) waits for it. Tell the user the run is waiting for their
approval: Actions → the run → **Review deployments** → `release` → Approve.

**Never approve it yourself.** Through the maintainer's account a session *could* call the
pending-deployments API, but the reviewer gate exists precisely so a human sees each publish.

## 6. Verify the release actually landed

Do not stop at "the tag was pushed".

```bash
gh run list --workflow=release.yml -L 1              # tag build started? verify-tag green?
gh run watch "$(gh run list --workflow=release.yml -L 1 --json databaseId -q '.[0].databaseId')"
gh release view "v$V" --json assets -q '.assets[].name'   # 4 tarballs + SHA256SUMS
npm view @tokuhirom/mutsu version                    # npm publish landed?
gh attestation verify "mutsu-v$V-linux-x64.tar.gz" -R tokuhirom/mutsu   # after downloading it
```

The GitHub Release should carry four `mutsu-vX.Y.Z-*.tar.gz` assets plus
`mutsu-vX.Y.Z-SHA256SUMS.txt`.

If `verify-tag` fails, the tag is wrong and nothing was published. Delete it
(`git push origin :refs/tags/vX.Y.Z`), fix the cause, and tag again.

If a later job fails, the bump commit and the tag are already on `main`, and npm or Docker may
already be published. Decide with the user whether to fix forward with a new patch version or to
delete and re-push the tag. Do neither silently.

## Release notes

Notes are auto-generated from the PRs merged since the previous tag, and grouped into sections by
`.github/release.yml`: 🚀 Features / 🐛 Bug Fixes / ⚡ Performance / 📝 Documentation /
📦 Dependencies / 🔧 Maintenance / Other.

GitHub sorts each PR into a section by its label:
- `.github/workflows/label-pr.yml` applies the category label from the PR title's
  conventional-commit prefix: `feat`/`fix`/`perf`/`docs`/`maintenance`, or `dependencies` for
  `*(deps):`.
- Dependabot labels its own PRs `dependencies`.

**So keep the `type:` / `type(scope):` PR title convention.** It is the only thing driving both
the label and the release-note section. A PR titled without a prefix falls through to "Other
Changes". The bump PR's own `chore(release):` title lands in Maintenance.

## One-time infra prerequisites (already done; do not undo)

- **Tag ruleset for `v*`.** Creation, update and deletion are restricted, and only the maintainer
  (repository admin) may bypass. This is what keeps a stray push from publishing a release
  (docs/security.md, "Repository settings these rules rely on").
- **Environment `release`.** Deployment branches and tags are limited to `main` and `v*`, and the
  maintainer is a required reviewer. npm's trusted publisher names this environment, so a
  `release.yml` edited on a branch cannot publish.
- **npm bootstrap.** npm only allows trusted publishers on a package that already exists. The
  first `@tokuhirom/mutsu` version was published interactively with 2FA
  (`npm publish --access public <tarball>`). Its GitHub Actions trusted publisher was then
  configured as user `tokuhirom`, repository `mutsu`, workflow `release.yml`, environment
  `release`. Every later tag publishes without a token.
- **macOS arm64** was `continue-on-error` until the vendored-libffi bump (ADR-0012) fixed its
  Mach-O CFI build. That `optional` flag is gone, so a macOS regression now fails the release
  loudly. **Do not "fix" macOS by weakening the Linux path.**
