# Bundled-library test-suite gate

mutsu ships several upstream Raku libraries verbatim ("batteries" — see
[BATTERIES.md](../../BATTERIES.md) and [vendor/README.md](../../vendor/README.md)):

- `vendor/zef/` — the Zef package manager that drives `mzef`
- `modules/<Dist>/` — every bundled battery: the TLS foundation (`OpenSSL`,
  `IO-Socket-SSL`), the HTTP client (`HTTP-UserAgent`) and its dependency layer.
  [`batteries.lock`](../../batteries.lock) is the authoritative list.

Their upstream **test suites are not vendored** (BATTERIES.md §3 — we ship only
`lib/` + attribution). So "does the bundled copy actually work under this mutsu?"
was previously only ever checked by hand. This gate makes it a **release
requirement**: a tag cannot publish unless every bundled library's upstream
tests still pass, at a recorded per-file baseline, against the *shipped* library
and the release `mutsu`.

## Where it runs

**At release — authoritative.** A release is cut by the manual
`tag-release.yml` workflow (`gh workflow run tag-release.yml -f version=X.Y.Z`;
see "Cutting a release" in `CLAUDE.md`), which bumps the version and pushes the
tag that fires `release.yml`. The `batteries` job there `needs`-gates the
publish job, so a regression against a shipped library blocks the release.

**Post-merge — early warning.** Release time turned out to be far too late to
*learn* about a regression. Nothing else ran the gate, so `Template::Mustache`
could sit on the whitelist at 6/13 for days; the drift only surfaced when it
took the v0.19.0 release run down (the tag was pushed, the publish was skipped).
The `test` job in `ci.yml` therefore also runs the gate on a push to `main`,
which attributes drift to a commit within minutes of the merge.

**On every PR.** The gate used to be skipped on an ordinary pull request — it
clones 17 upstream repositories, and the merge path was not supposed to depend
on the network. That trade turned out to be the wrong way round. Post-merge
detection tells you a battery broke, but by then the breakage is on `main`, and
the person who finds it is whoever next opens a batteries-touching PR — blocked
by a regression they did not cause, on a branch where it is hardest to attribute.
`DBIish/01-basic.rakutest` went from 35/35 to 27/35 exactly that way
(`news/2026-08/prelude-helper-not-block-lexical.md`): the offending PR did not
touch `modules/`, so its own CI never ran the gate.

So it runs on every PR now. The cost is what it always was — ~75s of suites, on
a job that has already built the release binary for roast — and a
documentation-only PR skips the whole job anyway.

The pin tests under `t/` are not a substitute. They cover bugs that have already
been found; the gate is the net for the ones that have not.

## Moving parts

| File | Role |
| --- | --- |
| `batteries.lock` | Which batteries, where their tests come from, the pinned upstream commit, and the extra `-I` paths each suite needs. |
| `batteries-whitelist.txt` | The per-file baseline: `name<TAB>testfile` for every test file that currently passes. Sorted. |
| `batteries-exclude.txt` | Files the gate must never run, same `name<TAB>testfile` shape. Skipped in both modes, so they can neither block a release nor enter the baseline. |
| `scripts/battery-testsuite.sh` | The harness. Fetches each suite at its pinned commit, runs it against the bundled library, and enforces (or, with `--update`, regenerates) the whitelist. |
| `release.yml` `batteries` job | Runs the harness on every release build; `needs` gates the publish job. |
| `ci.yml` `test` job, last step | Runs the same harness on every PR and every push to `main`. No path filter — see "On every PR" above for why. |

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
is `ok`, counting a `not ok … # TODO …` as passing.

A `# TODO` failure is an *expected* failure — TAP says the suite still passes,
and `prove` agrees. Upstream suites use it for assertions that depend on the
host: `NativeLibs`' `10-search.t` marks its "is there a versioned
`libmysqlclient`?" probe TODO, and raku fails that subtest on this machine too.
Counting it as a failure would make such a file ungateable even at exact parity
with raku. The verdict line reports the count (`PASS(1 todo)`) so a file quietly
turning its whole plan into TODOs is still visible.

The harness unconditionally sets `DBIISH_WRITE_TEST=YES` (harmless for every
other battery, which doesn't read it): without it, `DBIish`'s own
`CommonTesting` harness does a bare `skip-rest` covering the *entire* planned
count the moment a file reaches a write assertion, so the whole file trivially
"passes" without exercising any of its real code path. This is a stronger case
than the general `NETWORK_TESTING` policy below (an ordinary skip-gated
assertion still leaves the rest of its file exercised) — a variable that
degrades an entire file to a no-op is worth flipping on so the whitelist means
something.

**Each test runs with the fetched repo as its working directory**, which is how
`prove` / `zef test` run these suites. It matters: they reach for fixtures by
relative path (OpenSSL's `03-rsa.rakutest` does `slurp 't/key.pem'`). Running
them from the mutsu repo root instead makes such files die before their first
test and be miscounted as *library* failures rather than harness artifacts.

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

## What the gate does not run

The gate blocks a release, so a test whose verdict depends on a **third-party
service** being reachable and healthy must not be in it — an outage somewhere
else would block a release that has nothing wrong with it. Those files are listed
in [`batteries-exclude.txt`](../../batteries-exclude.txt) and are skipped
entirely, in both gate and `--update` mode.

The bar is deliberately narrow: the file must reach outside the machine
*unconditionally*. Most battery suites already guard their live-network
assertions behind `NETWORK_TESTING`, which the gate does not set, so they need no
entry. That was measured, not assumed — every whitelisted file was re-run inside
a loopback-only network namespace:

```sh
unshare -rn -- sh -c "ip link set lo up; cd <clone>; exec mutsu <-I…> <test>"
```

Only two files failed, and they are the two in the exclusion list. With them
excluded, **every file in the baseline passes offline** — the gate's verdict does
not depend on any third-party service. Re-run that check when adding a battery
whose suite talks to the network, and keep it true.

An excluded file is not a parking spot for a genuinely failing test: it must
still be run by hand (and it is, by the module's own record), it is simply not a
release blocker.

The gate itself still needs the network to *fetch* each suite at its pinned
commit — that is unavoidable setup, not an assertion. A fetch failure reports
`GATE ERROR` and exits 2, distinct from the `GATE FAILED` a real regression
produces.

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
