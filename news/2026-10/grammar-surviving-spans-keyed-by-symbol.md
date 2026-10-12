# The reduce-replay's surviving-span set is keyed by Symbol

`Grammar.parse(:actions)` builds the set of `(rule, from, to)` spans of the
successful match tree to find reductions made inside backtracked alternatives
(#12383). The set was keyed by the rule's resolved `&str`, which cost a
thread-local symbol-table lookup and a string hash per node; it is keyed by the
interned `Symbol` now. `bench-yaml-parse` (callgrind, `MUTSU_JIT=off`, second
run, same box): 354.65M -> 342.47M instructions (-3.44%), `take_repeated_entries`
16.95M -> 5.20M inclusive; allocations are unchanged (#12409).
