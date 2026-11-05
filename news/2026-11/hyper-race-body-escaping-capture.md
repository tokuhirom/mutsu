# hyper/race for bodies count as escaping captures

A `hyper for` / `race for` body runs the enclosing frame's inline bytecode on worker threads rather than a closure, so the escaping-capture analysis never saw its references. `compute_free_vars` now adds every own lexical a threaded `ForLoop` body names (except those the body declares itself) to the frame's escaping-capture set, so `@`/`%` containers get their declaration-site cell and workers reach them through it (ADR-16595 rule 2, slice 2).
