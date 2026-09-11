# The ecosystem parity numbers can now be refreshed from GitHub Actions

The ecosystem parity KPI ([#7785](https://github.com/tokuhirom/mutsu/issues/7785),
[ADR-0085](../../docs/adr/0085-ecosystem-testsuite-parity-measurement.md)) had one
practical problem: refreshing it required a specific machine. The harness needs
`bubblewrap`, a rakudo oracle, a release build and cores — so in practice only the
maintainer's 12-core box could produce a number, and a remote container (4 cores,
no `bwrap`) could not. `.github/workflows/ecosystem-sweep.yml` removes that
precondition: one **Run workflow** click measures a selection and opens the pull
request with the updated records.

```sh
gh workflow run ecosystem-sweep.yml -f scope=stale     # refresh what is out of date
gh workflow run ecosystem-sweep.yml -f scope=all -f history=true   # the corpus sweep, P2
gh workflow run ecosystem-sweep.yml -f scope=only -f only='BTree, Trie'
```

`scope` maps onto the harness's own selectors (`--stale`, `--all`, `--prefix`,
`--only`, `--status`), so the workflow adds no measurement semantics of its own —
it is an entry point, not a second implementation.

## What had to be engineered, not just wired up

A sweep fanned out across 27 hosted runners is not 27 independent sweeps. The
whole value of the KPI is that mutsu and rakudo ran against the same denominator
(ADR-0085 D2/D8), and three things would have quietly broken that:

- **One binary for the run.** The `build` job compiles `--release` once and every
  shard downloads that artifact. Building per shard would let 27 slightly
  different compilations feed one number.
- **One rakudo for the run.** `build` resolves the prebuilt release once and each
  shard installs *that* version. `install-raku.sh` otherwise picks "the newest
  right now", which for a multi-hour sweep can differ between the first shard and
  the last — a denominator moving inside a single measurement.
- **One ecosystem index for the run.** `build` fetches the fez + REA indexes once
  and seeds every shard's cache, so no two shards resolve different versions of
  the same distribution. Without this, `index-snapshot.json` would also describe
  only whichever shard happened to write it last.

`bwrap` is verified on each runner (Ubuntu 24.04's AppArmor restriction on
unprivileged user namespaces is lifted if it blocks the probe) and the job
**fails** rather than measuring unsandboxed — the sweep runs unaudited
third-party test suites, so there is deliberately no fallback.

## Two honesty guards

- **`measured.host` now distinguishes the machine.** It was `<uname>-<machine>`,
  which is `linux-x86_64` for both a runner and the maintainer's box — so D7's
  "a number produced elsewhere is identifiable" did not actually hold across
  those two. `MUTSU_ECO_HOST` overrides it and the workflow sets `gha-<image>-<n>c`.
- **A `history.tsv` row is refused unless the sweep earns it.** That row reports
  one `(mutsu commit, rakudo, host)` triple and one corpus-wide rate, so the
  workflow will not append one for a subset (`scope` other than `all`), for a run
  where a shard failed, or for a run whose records disagree about what measured
  them. The records still land in all three cases; only the chart point is
  withheld, and the run summary names the guard that fired.

## What this does not change

ADR-0085 D9 rejected a *scheduled* sweep, and that still stands — the workflow
has no `schedule:`, only `workflow_dispatch`. A full corpus run is ~20 CPU-hours
to track a number that moves at the speed of interpreter fixes; paying for it
weekly buys nothing. It is also not a gate: PLAN.md §1 B1's "working-module
regression CI" remains a separate, unstarted item. The ADR records this as a
dated amendment to D9 rather than a rewrite, because the decision was extended
(who needs the hardware), not reversed (when it runs).

> **Reversed the next day.** The sweep is scheduled nightly as of 2026-09-11 —
> the "~20 CPU-hours" above was an estimate, and the first real corpus run cost
> 78 minutes of wall time and ~5.5 hours of job time, a quarter of it. See
> `news/2026-09/ecosystem-sweep-runs-nightly.md` and ADR-0085 D9's second
> amendment. The paragraph stands as what was believed when this was written.

## Also in this change

`scripts/ecosystem-ci.py` holds the two decisions the workflow has to make —
`plan` (dispatch inputs → job matrix) and `provenance` (group records by the
triple that measured them) — with a `--self-test` covering both, so they are
debugged locally instead of one dispatched run at a time. It validates every
input at plan time, which is also what makes it safe for the workflow to
word-split the generated selector.

`provenance` doubles as the local runbook's answer to "is what I am about to land
uniform?", which §8 previously asked the operator to eyeball.

One documentation bug fell out of writing it: both `docs/ecosystem-parity.md` and
`ecosystem/README.md` suggested `--status regression`, but `regression` is a
per-*file* verdict — a record's `status` is `green` / `partial` / `red` /
`no_baseline` / `blocked_load` / `blocked_dep` / `skipped`, so that command
selected nothing. The examples now say `--status partial`.
