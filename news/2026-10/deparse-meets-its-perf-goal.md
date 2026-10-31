# `RakuAST::Deparse` dispatch meets its goal

Dispatching a `multi method` call copied every visible candidate that did not
bind it (`RakuAST::Deparse`: ~265 of 267) into a `Vec<(Symbol, MethodDef)>`,
a `MethodDef` clone being several `String` copies, only so that
`pick_method_winner` could build their rank keys for the tiers. The rivals are
now `Rival` handles, a share of the owner's `MethodOverloads` row plus an
index, so collecting them bumps a refcount each.

Callgrind, six DEPARSE snippets per round (profiling build, steady state):
658M -> 596M Ir (-9%). Since #15641 was filed (the pairwise tiering, #15672,
#15680, #15686, #15691, #15722 and this change): 3,556M -> 596M Ir per round
(-83%). Against rakudo, warm `"say 1 + 2;".AST.DEPARSE` x20 is 0.20-0.32 s vs
0.057-0.079 s (3.5-4x; the goal was 5x), and the first call is faster than
rakudo's (0.04 s vs 0.12-0.30 s, rakudo compiling `RakuAST::Deparse`).

`t/rakuast/rakuast-deparse.t` on a debug binary alone: 121 s -> 27 s (GC-stress
and JIT-stress settings 21 s each), inside the base budget; its x3 entry in
`scripts/run-t-test.sh` is no longer needed.
