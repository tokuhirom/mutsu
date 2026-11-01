# `Lock.protect` stops rescanning its block on every call

`$lock.protect: { ... }` runs a block literal inline in the calling frame. Before the run it
needs, for that block, which of the block's locals the closure captured from its env, which of
them the block assigns, and which global names it touches. That analysis was cached under the
closure's `SubData.id` -- and a closure literal gets a fresh id each time it is created, so
every `protect` call missed, rescanned the block's ops and left an entry nobody evicted (the
cache grew by one entry per call).

The env-independent part of the scan (assigned slots, global names in first-use order) is now
cached once per compiled body and shared by every closure instance of the literal; the
env-dependent part (which of those names this closure's captured env holds) is recomputed per
call from it, so the answer is exactly what it was. Also: the inline run resolved the enclosing
frame's slots through a `HashMap<&str, usize>` rebuilt per call from all of the frame's locals
(O(L) SipHash inserts); it now asks `CompiledCode`'s once-built slot index, and
`lexical_amp_call_eligible` builds its imported-alias key only when a name collides with a
routine.

Measured with callgrind (profiling build, warm, slope 100 -> 300 iterations of
`$l.protect: { $x++ }`): 25.4k -> see the PR for the final figure; the leak is gone (a
200,000-call loop no longer adds 200,000 cache entries).
