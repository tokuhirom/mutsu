# A Preserving-derived supply no longer loses, duplicates or reorders racing emits

The nightly stress run (#12831) went red on two files. `supplier-preserving-derived-replay.t`
raced: a `.map` of a `Supplier::Preserving` supply registered its transform tap *before*
carrying the source's backlog over, and `.Channel` on the derived supply took the backlog and
registered its forwarding tap as two separate steps, so an `emit` from another thread landing in
either window was delivered twice, ahead of the backlog, or never (the receiver then hung).

Recording an emission and handing it to the live taps is now one step
(`supplier_emit_collect`), serialized by a short emit gate against the two handoffs: the derived
supply registers its tap only once the source's backlog is drained (looping while emits keep
arriving), and `.Channel` takes the backlog and registers its tap under the same gate. A new
`t/concurrency/supply/supplier-preserving-derived-race.t` pins it (it hung ~3% of runs before).

`stack-deep-nesting-drop.t` timed out under the GC-stress configuration: a full scan every 1024
candidates walks the whole live chain, which is quadratic for a million nested arrays
(ADR-0003). Under `MUTSU_GC_EVERY_CANDIDATE` it now builds shallower chains; the default jobs keep
the full depths.
