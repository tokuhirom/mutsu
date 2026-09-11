# A sweep no longer overwrites a fix that landed while it ran

The corpus sweep measures for about an hour and a half. Interpreter fixes land in
that window — and a `todo:ticket` PR that repairs a bug re-measures the one
distribution it fixed, at a **newer** mutsu commit. The sweep's own record for
that distribution is stale before it is even committed.

`.github/workflows/ecosystem-sweep.yml` used to `cp -a` its records over a
checkout of the **dispatch-time** commit. Both halves of that were wrong: the
checkout was up to ninety minutes out of date, and the copy was unconditional, so
the sweep's older measurement would overwrite the fresher one — silently
re-opening a record that had just gone green, and producing precisely the merge
conflict that makes a 1600-file data pull request unpleasant.

## The rule

The `collect` job now checks out **main as it is now** and applies each record
through `scripts/ecosystem-ci.py apply`:

> **The newer mutsu commit wins.** A record says what mutsu did at one commit, so
> a record measured at a later commit is the more current answer, whoever
> measured it.

Ordering is `git merge-base --is-ancestor` on the two recorded commits — the real
commit graph, not a date heuristic, which is why the job checks out with
`fetch-depth: 0`. When git cannot order them (an unknown sha) the recorded date
breaks the tie; when even that ties, the record already on the branch is kept. A
record is never overwritten by one that cannot be *shown* to supersede it, and
the run summary reports how many were left alone.

This is also the merge-conflict answer. Records are one file per distribution, so
the only way a sweep can conflict with a sibling pull request is by touching the
same record — exactly the case the rule decides, before the branch is created.

## Why it is a script with a self-test rather than three lines of shell

The rule is the one piece of this workflow that can silently corrupt the ledger:
get the ancestry direction backwards and every sweep quietly reverts recent
fixes, with a green diff and no error. So it lives in `ecosystem-ci.py` behind
`--self-test`, which builds a throwaway git repository with three real commits and
checks the direction from both sides, equal commits (a commit must not count as
newer than itself), an unknown sha, the date fallback in both directions, and an
unreadable record on either side.

It was rehearsed end to end before shipping: a fake sweep measured `DAWG` and
`BTree` at an old commit while the branch carried a newer, green `BTree` — the
newer record survived, the sweep's `DAWG` landed, and the summary said "1 applied,
2 superseded".

## One consequence, recorded rather than hidden

A dist-fix PR measured on a dev box (often `"sandbox": "none"`, a different
`host`) now wins over a sandboxed `gha-*` measurement of the same distribution
while its commit is the newer one. Each record states its own sandbox and host, so
nothing is misrepresented, and the next sweep at a newer commit replaces it — or
a `scope: stale` run does, sooner.

## Measured while preparing the corpus sweep

Projected from the `D` shard's real numbers (128 distributions, 26.1 min of
measurement on a 4-vCPU runner), the whole 1624-distribution corpus is far more
evenly spread than the design's estimate assumed:

| | |
|---|---|
| largest shard | `A`, 154 distributions → ~31 min |
| total measurement | ~5.5 h of job time |
| wall clock at `max-parallel: 8` | ~1 h, plus a 5 min shared build |

No shard comes near the 6-hour job ceiling the fan-out exists to respect, so the
sharding is comfortable rather than marginal.
