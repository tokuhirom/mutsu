# A new shard directory made the ecosystem sweep discard everything it measured

The first real dispatch of `.github/workflows/ecosystem-sweep.yml`
(`scope: prefix`, `prefix: D`, [#7785](https://github.com/tokuhirom/mutsu/issues/7785))
measured all 128 `D` distributions on both interpreters in 26 minutes — and then
threw the results away one step later:

```
cp: -r not specified; omitting directory 'ecosystem/dists/D/'
```

`git status --porcelain` **collapses a wholly-new directory into a single entry**.
`ecosystem/dists/D/` had never been measured, so the 128 fresh records were
reported as one line, `?? ecosystem/dists/D/`, and the staging loop handed that
directory to `cp --parents`, which refuses a directory without `-r`. With
`set -euo pipefail` the step failed, the upload step was skipped for want of an
artifact, and the `collect` job correctly concluded "nothing changed" and opened
no pull request.

Two things were wrong, and the second is the one worth remembering.

## The bug

`git status --porcelain -uall` lists untracked files individually instead of
collapsing their directory. The staging and apply steps now use:

```sh
git -c core.quotePath=false status --porcelain -uall -z -- ecosystem \
  | tr '\0' '\n' | sed -n 's/^.\{3\}//p' | sort -u
```

`-z` plus stripping the three-character status prefix also keeps a path with a
space or a non-ASCII character intact — `awk '{print $NF}'` would have split the
first and git's default path quoting would have mangled the second. That matters
for the `_` shard, which exists precisely for distribution names that are not
ordinary ASCII words.

The same `-uall` is load-bearing in the `collect` job for a quieter reason: a
collapsed directory entry does not match the `\.json$` filter that feeds the
provenance check, so the records would have landed *unattributed* and silently
lost their `history.tsv` row.

## The real lesson: never let one bad path discard a measurement

A 26-minute measurement was destroyed by a single `cp` invocation, which is a
cost asymmetry the step had no business accepting. The staging loop is now
tolerant per path (one unexpected path costs one record and emits a warning), and
both the staging and upload steps run under `if: always()` so that a `Measure`
step which dies half way through still ships what it did measure. Records are
written per distribution as the sweep proceeds, `scope: stale` picks up whatever
is left, and the `collect` job already refuses a `history.tsv` row for an
incomplete run — so keeping a partial set is strictly better than discarding it,
and nothing downstream can mistake it for a complete sweep.

## What the discarded run did tell us

The measurement itself worked, and the log preserved its rollup — the first
ecosystem parity numbers ever produced on a hosted runner, over 128
distributions:

| status | dists |
|---|---|
| `blocked_load` | 45 |
| `green` | 27 |
| `partial` | 25 |
| `red` | 17 |
| `no_baseline` | 12 |
| `blocked_dep` | 2 |

`blocked_load` — a distribution whose own provided module does not even `use`
successfully — is the largest bucket at 35% of the shard, well ahead of the 27
that pass their whole baseline. That says the frontier for the `D` shard is
module *loading*, not assertion-level divergence, which is where P5's root-cause
grouping should start looking. These figures are a single shard on
`gha-ubuntu24-4c` and are recorded here as an observation, not as a KPI: the
records that would have carried their own provenance were the ones lost, and the
re-run is what lands them.

Everything else in the run was already green on its first attempt: the shared
release binary and pinned index artifacts, the `bwrap` probe on ubuntu-24.04
(plain `apt-get install bubblewrap`, no AppArmor relaxation needed), the pinned
rakudo install, and the vendored-`Test.rakumod` check.
