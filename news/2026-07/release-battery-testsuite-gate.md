# Bundled libraries' upstream test suites now gate the release

mutsu ships several upstream Raku libraries verbatim — Zef (`vendor/zef/`),
OpenSSL and IO::Socket::SSL (`modules/`). Their upstream test suites are
deliberately *not* vendored (BATTERIES.md §3 ships only `lib/` plus attribution),
so until now "does the bundled copy actually work under this mutsu?" was only
ever checked by hand, and only when someone remembered.

That check is now a **release requirement**. `release.yml` gained a `batteries`
job that the publish job `needs`: it fetches each battery's upstream test suite
at the commit pinned in the new `batteries.lock`, runs it against the *shipped*
library and the release `mutsu`, and fails the release if any test file listed in
`batteries-whitelist.txt` has regressed.

## Baseline, not all-green

The gate uses a **per-file baseline**, the same philosophy as
`roast-whitelist.txt`. Some battery suites have known gaps under mutsu today, so
requiring 100% green would simply block every release. Instead the whitelist pins
exactly the files that pass today, which means a release is blocked the moment a
*previously passing* battery test breaks, while the remaining gaps stay ordinary
follow-up work. The initial measured baseline is **11 of 18 test files**:

| Battery | Passing |
| --- | --- |
| Zef | 8 / 10 |
| OpenSSL | 2 / 7 |
| IO::Socket::SSL | 1 / 1 |

A file that starts passing is reported but not required; promote it with
`scripts/battery-testsuite.sh --update` and commit the diff.

## tagpr is manual now

The thoroughness was traded against release-PR churn: `tagpr.yml` no longer runs
on an hourly schedule and is **`workflow_dispatch`-only**. The release PR is only
consumed at release time, so continuously restating a pending CHANGELOG bought
nothing. Kick it by hand when you intend to release (`gh workflow run tagpr.yml`)
— and again right after merging the release PR, since tagpr creates the tag on
its next run.

See [docs/batteries/testsuite-gate.md](../../docs/batteries/testsuite-gate.md)
for the harness, the manifest format, and the re-vendoring workflow.
