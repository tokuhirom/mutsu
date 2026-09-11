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
not an ASCII letter). The rest of the filename is
`ecosystem_common.record_filename()`, whose self-test pins it:
`String::Utils` → `dists/S/String--Utils~aed281d9.json`.

- `::` becomes `--`, so the stem reads as the distribution name.
- Anything a filesystem or a GitHub artifact upload rejects (`: " < > | * ? % / \`)
  is percent-escaped — `App:Racl` → `App%3ARacl~...`. One rejected path fails an
  **entire** artifact upload, which once cost two shards every record they had
  measured.
- The `~<digest>` suffix makes the mapping **injective, case-insensitively**. It
  is not decoration: `::` → `--` alone is not injective over the real index
  (`Qwiratry::Location::HTTP` and `Qwiratry--Location--HTTP` both exist, and so do
  `WWW::CloudHosting::Hetzner` and `WWW--CloudHosting--Hetzner`), and a
  case-insensitive filesystem also collapses `CSV-AutoClass`/`CSV-Autoclass` and
  `Config::INI`/`Config::Ini`. Without the digest the first two pairs overwrote
  each other in the ledger and the last two were dropped by the artifact upload.

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
corpus without a sandbox) and a `raku` on PATH. Agent containers get both from
`.claude/hooks/session-start.sh`.

`index-snapshot.json` is corpus-level provenance, so a `--only` run deliberately
leaves it untouched — a single re-measured record must not date the whole ledger
to today's index. `--no-index-snapshot` does the same for a `--status` / `--stale`
re-measure.

**Or from GitHub Actions**, when no many-core box with `bwrap` is at hand: run
the [`Ecosystem sweep`](../.github/workflows/ecosystem-sweep.yml) workflow
(`gh workflow run ecosystem-sweep.yml -f scope=stale`). It builds mutsu once,
pins one rakudo and one index snapshot for the whole run, fans `scope: all` out
across the 27 shards, and opens the pull request itself — which auto-merges once
CI passes, because a diff of machine-generated measurements has nothing in it for
a reviewer to act on (`publish: branch` if you do want to inspect one first). Records it measures
carry a `gha-*` `measured.host`, so a CI number is never silently mixed with a
locally-measured one.

Full runbook, and what the workflow refuses to do:
[docs/ecosystem-parity.md](../docs/ecosystem-parity.md) §8.

## Asking what to fix first

```sh
scripts/ecosystem-tickets.py                   # root-cause clusters, most distributions first
scripts/ecosystem-tickets.py --issue b98eb9ef  # one cluster as a ready-to-file issue body
```

`scripts/ecosystem-tickets.py` reads these records back and groups them by root
cause, ranked by how many **distributions** each cause affects (never by failure
sites — one distribution with forty modules is one bug). Its output is filed as
GitHub issues rather than committed here; each body carries a stable
`eco-cluster: <id>` so a later sweep can tell a new cluster from a filed one.
Method, and the "verify a cluster before filing it" rule, in
[docs/ecosystem-parity.md](../docs/ecosystem-parity.md) §9.

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

## Current state — the corpus, measured at one commit

**The whole index is in.** [Run
34566091231](https://github.com/tokuhirom/mutsu/actions/runs/34566091231)
(2026-09-11, `scope=all`) measured all 1624 distributions at mutsu `1557d41`
against rakudo 2026.07, every one of its 27 shards green, and `summary.json`,
`summary.md`, `history.tsv` and `history.svg` exist as of that sweep. The
headline is **41.2%** dist parity (393 green of 953 graded); `file_parity` 53.6%
and `assertion_parity` 62.4% are the metrics that actually steer the work, since
a distribution goes green only when its last failing file does.

`site/content/ecosystem.json` still carries a `coverage` figure and the page
still leads with it. That is not vestigial: a *targeted* re-measurement
(`--only`, `--prefix`, `--status`) leaves the rest of the ledger at whatever
commit last measured it, so "how much of this ledger is current" stays a
question worth answering out loud.

`history.tsv` takes **one row per full sweep** (`--rollup --history`), and the
workflow refuses to append one unless `scope=all` *and* all 27 shards
succeeded — a row for a third of the corpus would put a point on the KPI chart
that is not comparable with the ones after it. It refused for exactly that
reason on the sweep before this one.
