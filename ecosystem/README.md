# `ecosystem/` — the parity ledger

Machine-readable measurements of **how mutsu does against rakudo on real zef
distributions**: each distribution's own test suite run under both interpreters,
with rakudo as the denominator.

- **Method and metric definitions**: [docs/ecosystem-parity.md](../docs/ecosystem-parity.md)
- **Why it is shaped this way**: [ADR-0085](../docs/adr/0085-ecosystem-testsuite-parity-measurement.md)
- **Tracking issue**: [#7785](https://github.com/tokuhirom/mutsu/issues/7785)

## What is here

| path | what it is |
|---|---|
| `dists/<S>/<Dist--Name>.json` | one record per distribution — the measurement |
| `index-snapshot.json` | which ecosystem index snapshot the run drew from (fez + REA, with digests) |
| `summary.json` / `summary.md` | generated rollup — **do not edit**, regenerate with `--rollup` |
| `history.tsv` / `history.svg` | one row per full sweep, and its chart — the KPI over time |

`<S>` is the uppercased first letter of the distribution name (`_` when it is
not an ASCII letter), and `::` is written `--` in the filename:
`String::Utils` → `dists/S/String--Utils.json`.

One file per distribution is not an accident. It is what keeps parallel PRs from
conflicting (the same reasoning as `news/`), and it makes re-measuring one
distribution a one-file change.

## Reading a record

```jsonc
{
  "dist": "BTree", "version": "0.0.4", "status": "partial",
  "axis": "pure",                       // pure / guts (nqp, MOP) / native (NativeCall)
  "measured": { "date": …, "mutsu_commit": …, "raku_version": … },
  "deps": { "resolved": [...], "unresolved": [] },
  "load": { "BTree": "ok" },            // per provided module
  "files": [
    { "path": "t/…", "raku": {…}, "mutsu": {…}, "cmp": "regression" }
  ],
  "totals": { "baseline_files": 2, "parity_files": 1, … }
}
```

`cmp` is the per-file comparison. **`regression` and `partial` are the
actionable ones**: rakudo passes the file and mutsu does not. `no_baseline`
means rakudo did not pass it either, so it is excluded from the KPI — it is
never charged to mutsu.

`status` rolls that up per distribution: `green` (every baseline file passes) ·
`partial` · `red` · `no_baseline` · `blocked_load` (a provided module does not
`use`) · `blocked_dep` (its dependency closure cannot be resolved, so it runs on
neither side) · `skipped`.

## Where this is published

`site/ecosystem.html` — searchable, one row per distribution, worst first —
is generated from this tree by `scripts/gen-ecosystem-manifest.py` and deployed
with the rest of the site by `.github/workflows/pages.yml`. It is a projection:
these records stay the authority.

## Re-measuring

```sh
scripts/ecosystem-sweep.py --only BTree          # one distribution
scripts/ecosystem-sweep.py --prefix A --jobs 8   # everything starting with A
scripts/ecosystem-sweep.py --status partial      # everything currently red
scripts/ecosystem-sweep.py --rollup              # regenerate summary.* and the chart
```

Requires `bubblewrap` (the sweep runs unaudited test suites and refuses to run a
corpus without a sandbox) and a `raku` on PATH.

**Or from GitHub Actions**, when no many-core box with `bwrap` is at hand: run
the [`Ecosystem sweep`](../.github/workflows/ecosystem-sweep.yml) workflow
(`gh workflow run ecosystem-sweep.yml -f scope=stale`). It builds mutsu once,
pins one rakudo and one index snapshot for the whole run, fans `scope: all` out
across the 27 shards, and opens the pull request itself. Records it measures
carry a `gha-*` `measured.host`, so a CI number is never silently mixed with a
locally-measured one.

Full runbook, and what the workflow refuses to do:
[docs/ecosystem-parity.md](../docs/ecosystem-parity.md) §8.

## Fixing one

Turning a red record green — check the distribution out, run both sides file by
file, fix the interpreter where the gap is bounded and file an issue where it
needs a complex feature — is the
[`ecosystem-dist-fix`](../.agents/skills/ecosystem-dist-fix/SKILL.md) skill.

To pick one **at random** rather than by name — and to do so with several agents
running at once — use
[`ecosystem-dist-roulette`](../.agents/skills/ecosystem-dist-roulette/SKILL.md)
instead. It draws uniformly from the actionable records here (a uniform sample is
what keeps the published figure honest; picking the cheapest-looking record games
it), takes a lock on
[#7884](https://github.com/tokuhirom/mutsu/issues/7884) so no two agents work the
same distribution, and then hands over to `ecosystem-dist-fix`. A distribution
has no issue of its own to carry a claim, which is why the lock lives on that one
board issue's comments rather than in this tree.

## Current state — a partial sweep, not the corpus

**The corpus sweep (P2) is in progress and these records cover only part of it.**
`site/content/ecosystem.json` carries a `coverage` figure and the page leads with
it, so a partial sweep can never publish a parity figure that reads as the whole
ecosystem's.

There is deliberately still no `summary.*` or `history.tsv`: `history.tsv` takes
**one row per full sweep** (`--rollup --history`), and appending a row for a
third of the corpus would put a point on the KPI chart that is not comparable
with the ones after it.
