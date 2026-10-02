# One unrelated `:=` no longer makes every scalar store probe the env

The scalar-store fast path has to know whether the slot it writes has a
`__mutsu_sigilless_alias::` key, because a store through an alias must walk to
its source. The answer was an env probe (~48 instructions) behind a
whole-program latch, so a single `my @unused := @data` anywhere made every
scalar store in the process pay the probe. That accounted for nearly all of
the +1.9% #9914 left on the #8748 repro.

The probe is now filtered per key: a bitset over alias-key symbols, filled at
the env tier funnel every key passes through, records which keys any env has
ever held. A store probes the env only when its own key's bit is set. The
filter over-approximates in the same direction as the old latch, so it can
only cost speed. Building it at the funnel also turned up an alias write in
`vm_misc_assign.rs` that bypassed `note_env_key`, which the whole-program
latches could miss; it now notes the keys.

On the #8748 repro (JIT on), one unrelated bind costs +0.61% instead of
+1.92%. The store path no longer reaches the env at all. What remains is the
per-slot key check itself, about 18 instructions per store, and only in
programs that made some alias. A program with no alias still answers with one
load.
