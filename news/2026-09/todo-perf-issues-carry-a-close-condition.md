# `todo:perf` issues carry a close condition

"Faster" has no end point, so `todo:perf` issues were only ever closed by a
maintainer's judgment call and the queue kept growing. As of 2026-09-24 (user
decision) every `todo:perf` issue states a **Goal (close condition)** — one
metric, where it is read, and a threshold — and is closed exactly when that
goal is met.

The goal follows from why the issue was filed:

- **slower than rakudo** — the ratio the filer wants (e.g. "faster than
  rakudo"), read from the named bench CI row;
- **a regression** — back within noise (3% unless stated) of the named
  pre-regression commit's row;
- **wrong complexity** — the correct order, shown by the family's
  `scripts/*-complexity-check.sh` case; a constant-factor speedup does not
  meet it.

An agent picking up a `todo:perf` issue writes a goal into it first if it has
none, announces the goal to the user (no confirmation needed), uses
`Refs #NNNN` for slices that fall short and `Closes #NNNN` for the change that
meets it, and never loosens the goal to close the issue. The rule lives in
`docs/issue-workflow.md`; the issue template, the `perf-tuning` skill,
`docs/complexity-annotations.md` and `CLAUDE.md` point to it.
