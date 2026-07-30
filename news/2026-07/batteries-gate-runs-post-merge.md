# The bundled-library gate now runs post-merge, not only at release

`scripts/battery-testsuite.sh` was wired into exactly one place: the `batteries`
job in `.github/workflows/release.yml`, which runs on a `v*` tag push or a manual
`workflow_dispatch`. Nothing ran it on a pull request or on a push to `main`.

So `batteries-whitelist.txt` could claim a file passed while it did not, for as
long as nobody cut a release. It did: between #5434 (2026-07-25, the slice that
took `Template::Mustache` to 13/13) and v0.19.0, three unrelated interpreter
changes took it down to 6/13 in silence. The gate itself worked — it failed the
v0.19.0 release run and the publish job was skipped, so the tag exists with no
GitHub Release behind it — but by then the drift was days and dozens of commits
old. The interpreter bugs are fixed in
[mustache-battery-regressions-fixed.md](mustache-battery-regressions-fixed.md);
this entry is about the detection gap.

## What changed

The `test` job in `ci.yml` now runs the same harness in two more situations:

- **On a push to `main`.** Drift is attributed to a commit within minutes of the
  merge instead of surfacing weeks later as a failed release.
- **On a pull request that touches the batteries** — `batteries.lock`,
  `batteries-whitelist.txt`, `batteries-exclude.txt`,
  `scripts/battery-testsuite.sh`, a shipped `modules/` tree, or `vendor/zef/`.
  Those are the changes whose whole point is to move the baseline, so the
  baseline should be checked before they land.

An **ordinary PR does not run it**, and its CI time is unchanged: the step is
skipped outright. That was the deciding constraint. The run clones 17 upstream
repositories, and the merge path should not acquire a network dependency for
every unrelated change.

## Why it is nearly free where it does run

The expensive part of the release-time `batteries` job is not the suites — it is
the `cargo build --release` in front of them. On a GitHub runner that job takes
~9 minutes end to end; the suites themselves take **~75 seconds** (measured
locally over all 17 batteries / 144 files). The `test` job has already built a
release binary for roast, so borrowing that build makes the added step cost
roughly a minute rather than a whole second job. `libssl-dev` (needed at runtime
by the `OpenSSL` / `IO::Socket::SSL` batteries' NativeCall) is installed inside
the conditional step, so an ordinary PR does not pay for that either.

## What this does not replace

The release-time gate stays authoritative — a regression still blocks a publish.
And the pin tests added alongside the Mustache fixes are not a substitute for
either: they cover bugs already found, whereas the gate is the net for the ones
that have not been. All three of the Mustache regressions came from changes that
had nothing to do with templating.
