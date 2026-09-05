---
name: cut-release
description: Cut a mutsu release — choose the version by semver judgment, fire the tag-release workflow, and verify that the tag build publishes all four tarballs, the npm package, and the GitHub Release. Use when asked to release, cut a version, tag a release, bump the version, or publish mutsu.
metadata:
  short-description: Release mutsu via the tag-release workflow
---

# Cutting a release

Releases are cut by **one manual trigger** — the `tag-release.yml` workflow. tagpr was removed
(2026-07-25): there is no release PR, no `CHANGELOG.md`, and no `minor`/`major` version-bump
label to apply on ordinary PRs.

## 1. Pick the version by hand

There is no label-driven *version* automation. Use semver judgment over what actually merged
since the last tag:

```bash
git fetch origin main --tags
git log --oneline "$(git describe --tags --abbrev=0)"..origin/main
```

- **patch** — fixes, roast progress, docs
- **minor** — a new user-visible feature
- **major** — a breaking change

## 2. Trigger the workflow

```bash
gh workflow run tag-release.yml -f version=0.18.0
```

(or Actions tab → "Tag release" → Run workflow). **No `v` prefix** — the workflow validates the
input against `^[0-9]+\.[0-9]+\.[0-9]+$` and fails otherwise.

## 3. What the trigger sets in motion

`tag-release.yml` (one job) bumps `version` in `Cargo.toml`, syncs the `mutsu` `Cargo.lock`
entry with `cargo update -p mutsu`, commits `Release vX.Y.Z` straight to `main`, and pushes the
`vX.Y.Z` tag. It does both through a **GitHub App token**, for two independent reasons: the App
is a configured bypass actor on the `main` ruleset (so the push is not rejected by
`required_status_checks`), and a tag pushed with the default `GITHUB_TOKEN` would **not** start
dependent workflows.

The tag push fires `release.yml`:

| Job | What it does |
| --- | --- |
| `build` | Builds `mutsu` + `mzef` for all four targets (Linux x64/arm64, macOS x64/arm64) and packages `bin/` + `share/mutsu/zef` tarballs. **All four are required** — none is `continue-on-error` any more. |
| `batteries` | Release gate: every bundled library's upstream test suite must still pass at its recorded baseline against the shipped library + this mutsu (`scripts/battery-testsuite.sh`). |
| `npm` | Builds the browser/WASM package and publishes `@tokuhirom/mutsu` via OIDC trusted publishing (`id-token: write`). npm records provenance automatically — **do not add a long-lived npm token.** |
| `release` | Downloads the artifacts, writes `mutsu-vX.Y.Z-SHA256SUMS.txt`, and creates the GitHub Release with `generate_release_notes: true`. |

`mutsu --version` reports `env!("CARGO_PKG_VERSION")`, so the `Cargo.toml` version the workflow
writes is the shipped version; the workflow keeps them coherent for you.

## 4. Verify the release actually landed

Do not stop at "the workflow was dispatched".

```bash
gh run list --workflow=tag-release.yml -L 1          # bump + tag pushed?
gh run list --workflow=release.yml -L 1              # tag build started?
gh run watch "$(gh run list --workflow=release.yml -L 1 --json databaseId -q '.[0].databaseId')"
gh release view "v0.18.0" --json assets -q '.assets[].name'   # 4 tarballs + SHA256SUMS
npm view @tokuhirom/mutsu version                    # npm publish landed?
```

The GitHub Release should carry four `mutsu-vX.Y.Z-*.tar.gz` assets plus
`mutsu-vX.Y.Z-SHA256SUMS.txt`.

If `release.yml` fails, note that **the version-bump commit and the tag are already on `main`** —
nothing rolls back. Decide with the user whether to fix forward with a new patch version or to
delete and re-push the tag; do not silently do either.

## Release notes

Notes are auto-generated from the PRs merged since the previous tag and **grouped into sections**
(🚀 Features / 🐛 Bug Fixes / ⚡ Performance / 📝 Documentation / 📦 Dependencies / 🔧 Maintenance
/ Other) by `.github/release.yml`. GitHub sorts each PR by label, and `.github/workflows/label-pr.yml`
applies the category label (`feat`/`fix`/`perf`/`docs`/`maintenance`, or `dependencies` for
`*(deps):`) from the PR's conventional-commit title prefix. Dependabot labels its own PRs
`dependencies`.

**So keep the `type:` / `type(scope):` PR title convention** — it is the only thing driving both
the label and the release-note section. A PR titled without a prefix falls through to
"Other Changes".

## One-time infra prerequisites (already done; do not undo)

- The release GitHub App must remain a **bypass actor on the `main` branch ruleset**, or
  `tag-release.yml`'s push to `main` is rejected by `required_status_checks`.
- **npm bootstrap:** npm only allows trusted publishers to be configured for an *existing*
  package. The first `@tokuhirom/mutsu` version was published interactively with 2FA
  (`npm publish --access public <tarball>`), then its GitHub Actions trusted publisher was
  configured as user `tokuhirom`, repository `mutsu`, workflow `release.yml`, allowed action
  `npm publish`. Every later tag publishes without a token.
- macOS arm64 was `continue-on-error` until the vendored-libffi bump (ADR-0012) fixed its Mach-O
  CFI build. That `optional` flag is gone, so a macOS regression now fails the release loudly —
  **do not "fix" macOS by weakening the Linux path.**
