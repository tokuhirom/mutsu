# `t/rakuast/rakuast-deparse.t` runs inside the base test budget

The x3 budget `scripts/run-t-test.sh` gave this file (34b7ed18, #15598) was a
stopgap for the per-dispatch pairwise tiering of `RakuAST::Deparse`'s 267
candidates. With the declared-type index and lazy tier walk (#15722) and the
shared rival candidates (#15729) the file takes 27 s on a debug binary alone
(121 s when #15641 was filed) and 21 s under each stress setting
(`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`,
`MUTSU_JIT=on MUTSU_JIT_THRESHOLD=2`), against a 90 s base budget, so the entry
is removed.
