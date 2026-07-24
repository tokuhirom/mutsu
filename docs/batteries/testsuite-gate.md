# Release-time bundled-library test-suite gate

mutsu ships several upstream Raku libraries verbatim ("batteries" — see
[BATTERIES.md](../../BATTERIES.md) and [vendor/README.md](../../vendor/README.md)):

- `vendor/zef/` — the Zef package manager that drives `mzef`
- `modules/OpenSSL/` — OpenSSL NativeCall bindings
- `modules/IO-Socket-SSL/` — TLS sockets (the HTTPS foundation)

Their upstream **test suites are not vendored** (BATTERIES.md §3 — we ship only
`lib/` + attribution). So "does the bundled copy actually work under this mutsu?"
was previously only ever checked by hand. This gate makes it a **release
requirement**: a tag cannot publish unless every bundled library's upstream
tests still pass, at a recorded per-file baseline, against the *shipped* library
and the release `mutsu`.

## Why at release time (and why tagpr is now manual)

Cutting a release is a deliberate act, so the expensive, network-dependent
suite run belongs there rather than on every merge. In exchange, `tagpr.yml` is
now **`workflow_dispatch`-only** — the release PR is no longer continuously
refreshed; you kick tagpr by hand when you actually intend to release. The
thoroughness moved from "restate the CHANGELOG constantly" to "verify the
batteries before we ship."

## Moving parts

| File | Role |
| --- | --- |
| `batteries.lock` | Which batteries, where their tests come from, the pinned upstream commit, and the extra `-I` paths each suite needs. |
| `batteries-whitelist.txt` | The per-file baseline: `name<TAB>testfile` for every test file that currently passes. Sorted. |
| `scripts/battery-testsuite.sh` | The harness. Fetches each suite at its pinned commit, runs it against the bundled library, and enforces (or, with `--update`, regenerates) the whitelist. |
| `release.yml` `batteries` job | Runs the harness on every release build; `needs` gates the publish job. |

## Running it

```sh
cargo build --release
# Gate mode (what CI runs): enforce the whitelist, non-zero exit on regression.
MUTSU_BIN=target/release/mutsu scripts/battery-testsuite.sh
# Update mode: re-measure and rewrite batteries-whitelist.txt.
MUTSU_BIN=target/release/mutsu scripts/battery-testsuite.sh --update
```

The harness runs each suite against the **bundled** library (`-I
vendor/zef/lib`, `-I modules/OpenSSL/lib`, …) — the clone provides only the
`t/` tests. A test file "passes" when it emits a TAP plan and every planned test
is `ok` with no `not ok`.

## Baseline, not all-green

The gate is a **per-file baseline** (the same philosophy as
`roast-whitelist.txt`), not an all-must-pass wall. Some battery suites have known
gaps under mutsu today (missing NativeCall surface, network/TLS-dependent
assertions). Whitelisting exactly the files that currently pass means:

- a release is blocked the moment a **previously passing** battery test breaks
  (a real regression), and
- suites with known gaps can still ride along — their passing files are pinned,
  and closing the remaining gaps is ordinary follow-up work, not a release
  blocker.

A file that starts passing is reported but not required; promote it into the
baseline by running `--update` and committing the diff.

## Re-vendoring a battery

When you bump a bundled library to a newer version, update its `commit` in
`batteries.lock` to the matching upstream commit, then:

```sh
scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt   # review!
```

A file that **dropped** out of the whitelist is a regression the new version (or
a mutsu change) introduced — fix it rather than accepting the smaller baseline.
A file that was **added** is progress to lock in.
