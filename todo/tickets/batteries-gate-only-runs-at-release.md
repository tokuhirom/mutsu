# The bundled-library gate only runs at release, so its whitelist silently rots

`scripts/battery-testsuite.sh` is wired into exactly one place: the `batteries`
job in `.github/workflows/release.yml`, which runs on a `v*` tag push or a
manual `workflow_dispatch`. Nothing runs it on a pull request or on a push to
`main`.

That means `batteries-whitelist.txt` can claim a file passes for weeks while it
does not. It happened: between #5434 (2026-07-25, the slice that took
`Template::Mustache` to 13/13) and v0.19.0, three unrelated interpreter changes
took it down to 6/13 and no CI run said a word. The regressions are fixed in
`news/2026-07/mustache-battery-regressions-fixed.md`, but the detection gap is
still open, and it applies to every battery, not just Mustache.

Why this is not a one-line change: the job builds a release binary, clones ~10
upstream repositories at pinned commits and runs their suites, with a 30-minute
timeout. Putting that on every PR roughly doubles CI cost and adds a network
dependency to the merge path. Options, cheapest first:

1. **Run it on pushes to `main` only.** Drift is then caught within one merge
   instead of one release, at one extra run per merge. Does not block a bad PR,
   but names the culprit commit immediately.
2. **Run it on PRs that touch the interpreter**, via a `paths:` filter on `src/`.
   Blocks the regression, but that filter matches most PRs anyway.
3. **Split the gate**: run a fast subset (the batteries whose suites are quick,
   which is most of them) on PRs and the full set at release.

Whichever is chosen, the release-time gate should stay as the authoritative one.
