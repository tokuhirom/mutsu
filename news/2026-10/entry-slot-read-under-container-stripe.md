# A deferred hash/array entry slot is read under the container stripe

`for %!h -> $p { $p.value }` hands out live entry refs, and every `$p.value` re-reads the shared map
through `EntryTerminal::peek`, which took no container stripe. Another thread inserting into the
same `has %!h` rehashed it under that read, so `t/concurrency/concurrent-attr-hash-for-loop-iteration.t`
died with a SIGSEGV on a release build roughly once in 300 runs under oversubscription (#13277; the
cores showed heap corruption: a write through a dangling `Vec` buffer, and a bounds panic whose
message formatting then faulted). `peek` now holds the node's stripe for the lookup, the read half of
the stripe the structural writers already take (ADR-0068 §16). On a release build with a Pointy-only
stress script, 0 crashes in 320 runs, against about 1 in 50 before. New pin:
`t/concurrency/concurrent-attr-hash-pair-value-read.t`.
