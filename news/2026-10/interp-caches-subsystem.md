# `Interpreter`'s 51 caches now live in one `ResolutionCaches` type

The second subsystem extraction under ADR-10779 (#10779) moved every
resolution and compile cache out of `struct Interpreter` into
`ResolutionCaches` (`src/runtime/resolution_caches.rs`). The moved fields
include the function/multi/method resolution memos, the call-lane tables,
`otf_compile_cache` and the map/grep/gather/carrier compile caches, together
with the generation counters that invalidate them. Code reaches them as
`self.caches.<field>`.

All of this is derived state. `clone_for_thread` already gave a spawned thread
empty caches; that policy is now stated once, in
`ResolutionCaches::fork_for_thread`, instead of as 51 separate lines in the
thread clone's struct literal. `Interpreter` went from 436 to 386 direct
fields. The step moved fields only and changes no behaviour. Collecting the
scattered cache invalidations into one entry point is left for a later step.
