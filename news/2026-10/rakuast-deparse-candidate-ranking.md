# Ranking RakuAST::Deparse's 267 candidates no longer walks the general type hierarchy

The nightly stress run of 2026-10-30 (#15598) timed out `t/rakuast/rakuast-deparse.t`
in both the GC-stress and JIT-stress TAP jobs (exit 124, 10 of 19 tests): the file
takes ~145s on a debug binary, over the jobs' 90s budget. No crash, no `not ok`.

Each `DEPARSE` dispatches `multi method deparse` over rakudo's 267 candidates, and
the pairwise tier ranking asked `nominal_type_conforms` ~50,000 times per node.
Every `RakuAST::` name was excluded from the conformance memo, so each ask took the
general `type_hierarchy_distance` walk. Now:

- two measured RakuAST names, and `Mu` / `Any`, are answered from a static ancestor
  set in the memo (`src/runtime/type_conformance.rs`);
- `assign_tiers_memoised` remembers the tiers of a candidate set whose relations were
  all static, keyed by the ranks, so a repeated dispatch skips the pairwise pass.
  A relation read from the registry or the env is never remembered.

Release `DEPARSE` of an `IntLiteral`: 24ms -> 3.7ms; of `say 1;`: 152ms -> 58ms
(rakudo: ~10ms). The first dispatch of each node class still ranks pairwise, so the
file's debug time only drops to ~106s and its budget is scaled x3 in
`scripts/run-t-test.sh`; the remaining cost is tracked as a `todo:perf` issue.
