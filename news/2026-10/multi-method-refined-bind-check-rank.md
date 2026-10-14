# A refined multi method candidate carries the bind-check flag, as a refined sub does

#13523 made a refined multi *sub* candidate (`where`, `subset`, `UInt`) tie a named-only one and skip the
optional-positional step, as in Rakudo. The multi *method* ranker still counted the refinement:
`multi method a(:$x = 1)` / `multi method a(UInt $s = 1, :$x = 1)` ran the second for `C.a`, and
`multi method b(UInt $s, $t?)` lost to `multi method b(UInt $s)`.

`method_rank_key` now sets `bind_check` for a candidate with a constrained parameter and drops its
optional-positional step, and `MultiRank::refinement_cmp` (shared with subs) finds the constrained-parameter
component through the new `constrained_slot` (0 for a sub, 1 for a method, whose component 0 counts
literals). A refined tie was already trial-bound, so it is not reported ambiguous.

Fixes #13522. Pinned by `t/oo/method/multi-method-refined-bind-check-rank.t` (every case compared with `raku`).
