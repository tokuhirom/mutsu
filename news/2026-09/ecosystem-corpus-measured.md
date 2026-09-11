# The ecosystem has a number: 41.2% of 1624 distributions

[Run 34566091231](https://github.com/tokuhirom/mutsu/actions/runs/34566091231)
(`scope=all`, `history=true`) swept all 27 shards green in 78 minutes and landed
1623 records. P2 of the parity campaign is done: every distribution in the fez
index has been measured at one mutsu commit against one rakudo, and the ledger
carries a `summary.json`, a `summary.md`, the first `history.tsv` row and its
chart.

```
date        mutsu_commit  raku_version  dists  measured  baseline_files  parity_files  file_parity  assertion_parity  dist_parity
2026-09-11  1557d41       2026.07       1624   1581      3072            1648          53.6         62.4              41.2
```

| metric | value | |
|---|---|---|
| `dist_parity` | **41.2%** | 393 green of 953 graded |
| `file_parity` | **53.6%** | 1648 / 3072 files |
| `assertion_parity` | **62.4%** | 92510 / 148161 assertions |

Status over the whole index: `green` 393, `red` 307, `partial` 253,
`no_baseline` 228, `blocked_load` 393, `blocked_dep` 43, `skipped` 7. (`green`
and `blocked_load` both landing on 393 is a coincidence; an independent recount
straight off the record files reproduces every field of `summary.json`.)

`dist_parity` is the headline because it is the question a user asks — *does my
module work?* — and it is brutal by construction: a distribution counts as green
only when its **last** failing test file passes. `file_parity` and
`assertion_parity` are the ones that move while work is in progress, which is why
they steer the queue.

## What the three attempts cost, and why the logs mattered

The corpus took three runs. The first threw away shard `D`'s 128 records after 26
minutes (`git status --porcelain` collapses a wholly-new directory into one
entry, `cp --parents` refused it, `set -e` did the rest). The second asked for
all 27 shards and came back with 1423 distributions — which *looks* like a
near-complete corpus and was not:

- shards `A` (154) and `S` (109) uploaded **nothing**: `upload-artifact` rejects
  a colon and fails the whole artifact over one path, and `App:Racl` /
  `Slang:Date` carry a single colon the `::` → `--` mapping never saw;
- shard `C` reported **success** having died at distribution 93 of 124 on
  `tarfile.AbsoluteLinkError`;
- the filename rule was not injective, so distinct distributions were sharing
  records.

None of that appeared in a run's conclusion. It came out of reading 27 shard
logs, which is the habit this campaign has to keep: **read a corpus run's logs
even when it says success.**

This run is the evidence the fixes work, and it sharpened one of them. The
per-distribution `try/except` caught `AbsoluteLinkError` **three** times, not
once, and each one sat in a different shard — `Collection::Plugin::Development`
in `C`, `Gnome::Gtk3` in `G`, `Sway::PreviewKeys` in `S`. Under the old code those
three would have truncated three shards, one of which had already lost
everything to the colon. Seven distributions are `skipped` in total: three for
that reason, four with no `META6.json` in the tarball.

## Cost

27 shards at `max-parallel: 8` is three waves: 78 minutes wall, about 5.5 hours
of job time, with `C` the heaviest single shard at 43 minutes for 124
distributions. That is an order of magnitude inside the design's "~20 CPU-hours,
2.5 hours wall" estimate, and comfortably a run-it-on-demand budget.

The data PR used to be the expensive part, for a reason that had nothing to do
with sweeping: a ~1300-file records diff tripped `ci-docs-only.sh`'s 300-file
threshold and paid for five build jobs. With completeness derived from the pull
request's own `changed_files` instead of guessed from a count, the PR merged **26
seconds** after it opened.

## Next

P5: the ledger now says where to start. Of the 393 `blocked_load` records, 53 are
`raku_also_fails` — rakudo cannot load them either, so they are not mutsu's to
fix — and the remaining 340 normalise to 133 distinct load errors whose top ten
cover a third (110 of 340): `X::Redeclaration` on a routine (17), "needs parens to
avoid gobbling block" (15), `No such method` (14), a cluster of parse errors,
`Could not find QAST in:` (8), `Unknown function: HAS` (8), slang activation for
a grammar rule override (8). The 560 `red`/`partial` records have a far longer
tail — 513 first failures in 336 distinct shapes — so the load axis is where
exhaustive grouping pays and the file axis wants sampling.
