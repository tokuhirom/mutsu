# Grammar actions run when their subrule reduces

A subrule's action method now runs at the moment the subrule matches, not after
the whole parse, as in Rakudo: a `{ ... }` block placed after `<a>` sees `a`'s
action effects and `$<a>.made`, a reduction abandoned by backtracking keeps its
effects in order, and a failed parse leaves the effects of the reductions that
happened. This replaces the post-parse replay of backtracked and failed
reductions, the deferred-repeat flushing and the reduce-time `$*` copy-run.
(#14517, ADR-7576)
