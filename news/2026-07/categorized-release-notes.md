# Categorize release notes by conventional-commit type

The auto-generated GitHub Release notes are now grouped into sections instead of
one flat "What's Changed" list. `release.yml` already passes
`generate_release_notes: true`; GitHub sorts each merged PR into the first
category in `.github/release.yml` whose labels the PR carries:

- 🚀 Features (`feat`)
- 🐛 Bug Fixes (`fix`)
- ⚡ Performance (`perf`)
- 📝 Documentation (`docs`)
- 📦 Dependencies (`dependencies`)
- 🔧 Maintenance (`maintenance`)
- Other Changes (anything else)

## Labels without hand-labeling

mutsu PRs follow the `type:` / `type(scope):` title convention rather than
carrying labels, so a new `.github/workflows/label-pr.yml` derives the category
label from each PR's title (on open / edit / reopen) — the author writes nothing
extra. `feat!:` and `fix(scope)!:` breaking-change markers are handled, and any
`*(deps):` bump maps to `dependencies`. Dependabot is skipped: it labels its own
PRs `dependencies`, so dependency bumps stay in their own section instead of
being mixed into Maintenance.

`label-pr.yml` labels every PR opened from this repo (a `pull_request` workflow
runs from the PR's own head branch, so it even labeled the PR that introduced
it). PRs merged *before* it existed stay unlabeled and fall under "Other Changes"
in the first release that spans them.
