# ADR-0085 — The ecosystem KPI is per-distribution test-suite parity against rakudo

- Status: Proposed
- Date: 2026-09-10
- Issue: [#7785](https://github.com/tokuhirom/mutsu/issues/7785)
- Operations manual (the "how"): [docs/ecosystem-parity.md](../ecosystem-parity.md)

## Context

mutsu's stated goal is a batteries-included Raku implementation, and its
positioning claim is *compatibility*: real ecosystem code runs. Today the
project has no number for that claim.

What exists measures something narrower:

- **roast** (`roast-whitelist.txt`, 1436/1464) is at its ceiling and is a
  *language spec* measure, not an *ecosystem* one. PLAN.md §3 already records
  that it is no longer the productive axis.
- **The batteries gate** (`scripts/battery-testsuite.sh`, `batteries.lock`) runs
  the upstream suites of the ~40 dists mutsu *bundles*. It is a regression gate
  on a hand-picked set, not a survey.
- **The dist-compat sweep** (`scripts/dist-compat-sweep.py`,
  [docs/dist-compat-sweep.md](../dist-compat-sweep.md)) samples random fez dists
  and asks only whether `use <module>` succeeds. Its own "Planned deepenings §1"
  names the missing level: *run each dist's own test suite*. Its `--run-tests`
  flag is a prototype of exactly that, but it is a sampler (random seed, first
  failure wins, results land in `tmp/`), not a ledger.

So the question a user actually asks — **"does my module work on mutsu?"** — has
no answer in the repository, and the project has no KPI that moves when real
compatibility improves.

Issue #7785 asks for that: run every zef distribution's test suite on both
rakudo and mutsu, treat the difference as the headline KPI, keep the data in the
repository in machine-readable form with the versions it was measured at, allow
re-measurement per distribution (and per first letter), and publish it.

The issue also sets a precondition: *"This campaign should only begin after you
start using Test.rakumod."* That precondition is **met as of 2026-09-10**: a bare
`use Test` resolves to rakudo's unmodified `modules/Rakudo-Core/lib/Test.rakumod`
and there is nothing else it could load — the native TAP provider was retired
that day ([#7566](https://github.com/tokuhirom/mutsu/issues/7566)) and the
`MUTSU_REAL_TEST` switch went with it (`modules/Rakudo-Core/README.md`).

The precondition matters because it is load-bearing for every number this
campaign produces. A natively-provided `Test` would be comparing mutsu's own
assertion implementation against rakudo's, so each side would be counting
`ok` lines its *own* harness decided to emit. Since the two sides now run the
same `Test.rakumod` source, a TAP line means the same thing on both.

## Decision

Adopt **per-distribution test-suite parity against a same-host rakudo run** as
mutsu's ecosystem KPI, measured by a dedicated, exhaustive, re-runnable harness
whose per-distribution results are committed to the repository.

The eight decisions below are the ones that are costly to reverse once thousands
of records exist. Everything else — CLI spelling, output formatting, phasing —
is operational and lives in [docs/ecosystem-parity.md](../ecosystem-parity.md).

### D1. The unit of measurement is a distribution; the unit of comparison is a test file

A "zef module" in the issue is a **distribution** (`Dist::Name` in the fez
index): the thing that has a version, a tarball, a `META6.json`, and a test
suite. A *module* is one entry in its `provides`. The vocabulary is fixed that
way everywhere in this campaign — records are keyed by distribution.

Inside a distribution the comparable unit is one **test file** run as one
process, because that is the unit both `prove` and `zef test` use and the unit a
TAP plan describes. Assertions are counted within a file but are not the unit:
`ok 7` in rakudo and `ok 7` in mutsu are only comparable when the file reached
the same point.

### D2. Rakudo is the denominator, run on the same host in the same sweep

The recorded number is never mutsu's absolute pass rate. It is always a ratio to
a rakudo run of **the same distribution version, the same test file, the same
dependency set, the same working directory, the same environment, and the same
timeout**, executed in the same sweep on the same machine.

This is not fussiness. A large fraction of ecosystem suites fail for reasons
that have nothing to do with the interpreter: they need the network, a database,
a specific OS, an author-only fixture, or they are simply broken at their latest
published version. An absolute pass rate would report all of that as mutsu's
failure and would drift with the host. A ratio to rakudo cancels it.

Consequence: a test file rakudo does not pass cleanly is **excluded from the KPI
denominator** and recorded as `no_baseline`. It is never counted as a mutsu
failure, and never as a mutsu success.

### D3. Three parity numbers, and `file_parity` is the headline

Let *B* be the set of test files that rakudo passes cleanly in this sweep.

| metric | definition | role |
|---|---|---|
| `file_parity` | `count(f in B where mutsu passes f) / count(B)` | **the KPI** — the number to move |
| `assertion_parity` | `sum over B of min(mutsu_ok, raku_ok) / sum over B of raku_ok` | fine-grained progress; moves even when no file flips |
| `dist_parity` | `count(dists whose whole baseline set passes on mutsu) / count(dists with a non-empty baseline set)` | the user-facing "which modules work" number |

All three are reported, and both sides' raw TAP counts are stored per file, so
any other aggregate can be recomputed later **without re-running anything**.
That is the reason the store keeps per-file counts rather than a rolled-up
verdict: a metric definition we change in six months must not cost a re-sweep.

### D4. Dependencies are resolved as a flat `-I` closure from the ecosystem index — never installed

For each distribution, resolve the transitive closure of `depends` +
`test-depends` **offline, from the fez index itself** (its `provides` map covers
9214 module names), download and extract each dependency tarball, and pass
`-I <dep>/lib` for every member of the closure — the *same* list to both
interpreters.

Rejected alternatives:

- **`zef install` for rakudo and `mzef install` for mutsu.** Two different
  installers, two different repository formats, precompilation on one side and
  not the other: the two sides would no longer be running the same code, and a
  bootstrap failure in `mzef` (itself running on mutsu) would silently become a
  mutsu compatibility number.
- **Installing into a shared site repository.** Same asymmetry, plus it makes a
  run depend on the order dists were installed in.

A flat `-I` closure is deterministic, cacheable, identical for both
interpreters, and needs no package manager at either end.

**The index must be fez + the [Raku Ecosystem Archive](https://github.com/Raku/REA),
not fez alone.** Measured on 2026-09-10 snapshots of both, resolving the
*transitive* closure over the 1623 fez distributions:

| index | closures that resolve | blocked |
|---|---|---|
| fez only | 1164 (72%) | 459 |
| fez + mutsu's bundled batteries | 1260 (78%) | 363 |
| **fez + REA** | **1584 (98%)** | 39 |
| fez + REA + bundled batteries | 1584 (98%) | 39 |

(The 85% figure that a first-level-only count gives is misleading for a
resolver that recurses to a fixed point; 72% is the operative number.) The
blockers fez alone cannot supply are legacy p6c/CPAN-era dependencies that
never moved to fez — `File::Which` (69 dependents), `Hash::Merge` (60),
`Distribution::Builder::MakeFromJSON` (58), `Getopt::Long` (52) — and REA
carries all of them. The 39 that remain are almost entirely malformed
`depends` entries in the source `META6.json` (prose fragments such as `has`,
`path`, `as`), not real distributions.

Note the fourth row: **once REA is in, mutsu's bundled batteries add nothing
to coverage.** That is what makes D5 cheap rather than a sacrifice.

### D5. A distribution whose dependency closure cannot be resolved is skipped on *both* sides

When the closure cannot be completed (a dependency published only to a legacy
ecosystem, or genuinely absent), the distribution is recorded as `blocked_dep`
and is **not run on either interpreter**.

The temptation is to run it anyway and let mutsu's bundled batteries
(`modules/`, resolved below `-I` in the precedence chain) satisfy the gap. That
would be a real advantage of the product — and exactly the wrong thing to put in
a parity number, because rakudo would fail on a missing dependency while mutsu
passed, and the KPI would rise without any compatibility improving.

The value of the bundle is instead recorded *separately*: each `blocked_dep`
record notes whether a bundled battery would have supplied the missing
dependency (`bundled_would_supply`). That is a batteries statistic, reported as
such, not folded into parity.

The measurement in D4 shows this rule costs almost nothing: with fez + REA the
bundle rescues **zero** additional distributions (1584 either way), and the
population it applies to is 39 distributions, most of them malformed metadata.
The rule was worth stating anyway — without it the KPI would have a standing
way to rise without any compatibility improving — but it is not a trade.

### D6. The store is one JSON file per distribution, sharded by first letter

```
ecosystem/dists/<S>/<Dist--Name>.json
```

with `<S>` the uppercased first character (`_` when not an ASCII letter) and
`::` written `--` in the filename.

One file per distribution is chosen for the same reason `news/` is one file per
entry and for the reason `docs/dist-compat-sweep.md` had to adopt one-row-per-dist
tables: **a shared file conflicts with every parallel PR.** Here it also makes
the issue's two re-measurement modes fall out of the layout for free —
re-measuring one distribution rewrites exactly one file, and "all modules
starting with A" is one directory.

Derived artifacts (`ecosystem/summary.json`, `ecosystem/summary.md`,
`site/content/ecosystem.json`) are **generated from that tree** and never hand
edited, so a conflict in them is resolved by regenerating rather than by
merging.

### D7. Every record carries the versions it was measured at, and staleness is visible

Each record stores the mutsu commit and version, the rakudo version and backend,
the distribution version, the index snapshot it was drawn from, the dependency
closure digest, the host platform, and the sandbox mode. A record whose
`mutsu_commit` is not an ancestor of the current `main`, or whose `raku_version`
differs from the current oracle, is **stale by definition** and the tooling can
select exactly those for re-measurement.

Timestamps are recorded at **date** granularity. A re-run that reproduces the
same verdicts on the same day therefore produces a byte-identical file and no
diff; a sweep on a new day rewrites the records it executed with a one-line
provenance delta, which is a truthful "re-verified at commit X" record and an
acceptable churn budget for a weekly-or-slower full sweep.

### D8. Both interpreters run sandboxed and identically configured, and the harness enforces it

Running the test suites of ~1600 unaudited distributions is arbitrary code
execution at scale. Both sides run inside the same bubblewrap confinement the
dist-compat sweep already uses (no network, read-only filesystem, throwaway
HOME, own PID namespace, rlimits) — *both* sides, because a network-dependent
suite must fail symmetrically or it would show up as a mutsu regression.
`--sandbox none` exists for a single distribution the operator already trusts;
a corpus sweep without a sandbox is not a supported mode.

Two environment rules are part of the measurement contract and are enforced by
the harness at startup, not left to the operator:

- `MUTSU_FUDGE` must be **unset** — it is roast-only, and a stray `#?rakudo skip`
  in a dist would silently drop the next statement.
- `use Test` must resolve to the vendored `Test.rakumod` on the mutsu side, not
  to a native provider or to a copy shadowed by the `-I` closure. The provider
  is retired, so this is a guard against regression rather than a switch, but a
  sweep that cannot confirm it aborts rather than publishing a flattered
  number.

## Consequences

- The project gains a compatibility number that is defensible, reproducible, and
  moves when real compatibility improves — and a user-facing answer to "does my
  module work on mutsu?".
- `scripts/dist-compat-sweep.py` keeps its job as the **diagnostic sampler**
  (load-level buckets, root-cause grouping into `TODO_dist/TICKETS.md`); the new
  harness is the **exhaustive ledger**. The two share one implementation of the
  index reader, the tarball cache, the sandbox wrapper and the TAP parser, so
  the security-relevant code exists once.
  [docs/dist-compat-sweep.md](../dist-compat-sweep.md) "Planned deepenings §1"
  is fulfilled by this campaign and points here.
- The `regression` records become the highest-volume source of real,
  impact-ordered interpreter bugs the project has, at a moment when roast is
  mined out (PLAN.md §3). They feed the existing queue in the shape
  `scripts/dist-compat-tickets.py` already produces — root cause first, dists
  affected as the impact count — rather than a new process.
- A rakudo upgrade moves the denominator. That is recorded per record and noted
  in the KPI history, and is the accepted cost of D2.
- Suites that need the network are invisible to the KPI (they fail on both
  sides, landing in `no_baseline`). This is a known, recorded blind spot, not a
  silent one.
- Committing ~1600 records is a few megabytes of JSON and a bounded per-sweep
  diff. Accepted in exchange for the data being *in the repository*, which the
  issue requires and which makes it browsable, diffable and reviewable.

## Alternatives considered

- **Keep sampling instead of sweeping the corpus.** A random sample answers
  "how are we doing" but never answers "does *my* module work", which is half of
  what the issue asks for, and its number jumps with the seed.
- **Store the data on a side branch** (the `bench-data` pattern). Cheaper diffs,
  but the issue says *in the repository*, and a side branch is invisible to
  someone browsing the project or reviewing a PR that changes compatibility.
- **Measure only load success (extend the existing L0 sweep to the whole
  corpus).** Much cheaper, and genuinely useful — but "it loads" is not "it
  works", and the gap between the two is precisely the interesting part.
- **Publish an absolute pass rate.** Simpler to explain, but it reports the
  ecosystem's own breakage and the host's limitations as mutsu's failure, and it
  cannot be compared across machines or across time.
