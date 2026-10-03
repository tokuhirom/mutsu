# An unrelated `:=` no longer slows down local reads

One `:=` anywhere in a program used to switch off the fast local-variable read
for the whole run, in every frame, with the JIT on or off. On the #8748
repro, a single `my @unused := @data;` that the hot loop never reads cost
+21.7% instructions with the JIT on.

Three changes removed that cost: the `GetLocal` latch no longer counts cells
(#10706, ADR-0097 §15), and the scalar-store side lost its own process-wide
alias latch (#10765, #10836). The repro now costs +0.08% with the JIT on and
+0.06% with it off, which is the bind statement itself.

`MUTSU_VM_STATS=1` now prints `local_read_spoilers=` on its `jit:` line. A
nonzero value means the fast read is off for the rest of the run. It is 0 for
every benchmark and for `use Test`, so test files and real workloads get the
fast read too. A new integration test keeps it that way. The unused
`rebind_target_slots` compile-time record is gone. (#9914)
