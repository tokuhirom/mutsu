# A loop's state sync and a local store no longer scale with the `state` declarations of the whole chunk

After every iteration a `for` loop synced its body's `state` variables, and
`sync_state_locals_in_range` tested every `state` local of the chunk for an
initialiser inside the loop (O(t) per iteration, t = state locals anywhere in the
chunk). Independently, `publish_state_local` ran on every write to a local and
located the slot's state key by walking `state_locals`, another O(t) per store
(#11347).

Both now go through the chunk's op-scan index: `for_each_state_local_in_range`
binary-searches the `StateVarInit` ips of the range, and `state_key_for_slot` is a
hash probe. The loop-entry reset (`reset_state_locals_in_range`) uses the same
query. A loop with `k` state declarations in its body pays O(k) per iteration
regardless of the rest of the chunk.

callgrind, release, JIT off, `for ^2000 { $c = -> { 1 } }` after N top-level
`state` declarations, per-iteration cost (loop minus the same program with a
zero-trip loop): N = 1000 -> 2000 was 59.7k -> 109.2k Ir (1.83x), now 10.1k -> 10.9k
Ir (1.07x). `scripts/vm-complexity-check.sh "for iteration vs chunk state locals"`
covers it.
