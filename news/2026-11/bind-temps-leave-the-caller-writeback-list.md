# Subscript bind temps no longer pile up in the caller-writeback list

Passing a sigilless term bound to a subscript (`my \hit = $cache{key}; absent(hit)`)
recorded a writeback for the compiler's per-site bind temp
(`__mutsu_bind_index_ref_N`). That name is an env name of the frame that made
it, never a local of any frame, so the rw drain moved it into the
retain-on-miss caller-writeback list, where it stayed for the rest of the
frame chain and every later call drained it again. Such temps are now dropped
instead of retained. On Monad's `t/14` chain loop this is ~0.9% fewer
instructions per iteration; the remaining entry there is the module's own
`$type-cache`.
