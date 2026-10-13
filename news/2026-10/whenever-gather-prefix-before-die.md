# A dying gather source delivers its prefix first

`Supply.from-list(gather { take 1; die "bang" })` now emits `1` before it quits,
so a `whenever` over it prints `(1 q:bang)` as `raku` does (#13318). `from-list`
pulls the gather one element at a time, so the values taken before the `die`
are kept. The pull of a gather body also no longer inherits an enclosing
condition-driven loop's "defer the suspension to the loop boundary" flag
(`lazy_take_boundary_defer`): inside a `supply` block it made the body run on
past the take limit, straight into the `die`.
