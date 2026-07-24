# Drop tagpr for a single-trigger release workflow

Releases are now cut by one manual GitHub Actions trigger instead of the
two-phase tagpr dance. tagpr had been switched to `workflow_dispatch`-only to
stop it rewriting the release PR on every merge, but that also removed its
"tag after the release PR merges" step — so a release meant running the same
workflow twice (dispatch to refresh the PR, merge, dispatch again to tag). If a
release is a manual act either way, tagpr earned its keep no longer.

## What replaces it

A new `.github/workflows/tag-release.yml` takes a `version` input and does the
whole thing in one run:

```
gh workflow run tag-release.yml -f version=0.18.0
```

It bumps `Cargo.toml` and the `mutsu` `Cargo.lock` entry to that version, commits
straight to `main` via a GitHub App token, and pushes the `vX.Y.Z` tag. The tag
push fires the existing `release.yml`, which already builds all four target
tarballs (Linux x64/arm64, macOS x64/arm64), runs the batteries gate, and
publishes the GitHub Release with auto-generated notes (`generate_release_notes:
true`, from the merged PRs since the previous tag). tagpr was never required for a
working release — `release.yml` creates the Release itself — so all it was still
doing was version bookkeeping, which the new workflow now does at the moment of
release.

## Removed

- `.github/workflows/tagpr.yml` and `.tagpr`.
- `CHANGELOG.md` (5679 lines): `generate_release_notes` produces the per-release
  notes from PRs, so a separately curated changelog was redundant churn.
- `.github/release.yml`'s tagpr-label exclusion (the file only held that).
- The `tagpr-from-*` skip condition in `ci.yml` (there is no release PR now).
- The `minor`/`major` version-bump label convention in `CLAUDE.md`: the version
  is chosen by hand at release time, not aggregated from PR labels.

## Fixed along the way

`mutsu --version` printed a hardcoded `mutsu 0.1.0` regardless of the real
version — a plain bug that also meant nothing consumed the `Cargo.toml` version.
It now reports `env!("CARGO_PKG_VERSION")`, so the version the release workflow
writes into `Cargo.toml` is the version the shipped binary reports. `Cargo.toml`
was also corrected from a stale `0.16.0` to the actually-released `0.17.0`.

## Infra prerequisite

The workflow pushes the version-bump commit directly to `main`, which the branch
ruleset's `required_status_checks` rule would otherwise reject. The release
GitHub App was added as a bypass actor on the `main` ruleset so the push
succeeds. If that bypass is ever removed, the release workflow's push to `main`
will fail.
