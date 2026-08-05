# JIT Tier A lacks the bitwise opcodes — RIPEMD's hot loop never goes native

`Digest::RIPEMD`'s remaining throughput gap (the last lever for
`t/ripemd.t`'s 120s batteries-gate budget, see
`todo/tickets/digest-ripemd-start-per-block-overhead.md`) is NOT spawn
overhead anymore — it is that the 80-round compression loop never enters
JIT-compiled code. Measured on the small repro (2026-08-05, release build,
after clone-slimming slice 6):

```sh
MUTSU_VM_STATS=1 ./target/release/mutsu -I modules/Digest/lib \
  -e 'use Digest::RIPEMD; say rmd160("a" x 100_000).elems'
# [mutsu vm-stats] jit: compiles=0 entries=0 bailouts=3126 ...
# [mutsu vm-stats] jit bailout opcodes (top 1): BitShiftLeft=3126
```

Zero compiles, zero entries: every hot chunk (2 per block x 1563 blocks)
bails out, and the first blocking opcode is always `BitShiftLeft` — RIPEMD's
rotate is built from `+<`/`+>`, so the shifts appear before anything else
exotic in each chunk.

## The fix

Add the bitwise family to the Tier A support tables in
`src/vm/vm_jit_support.rs`:

- `OpCode::BitAnd` / `BitOr` / `BitXor` / `BitShiftLeft` / `BitShiftRight`
  (`src/opcode.rs:91-95`). None of them is currently in `noarg_shim` (the
  dedicated `(interp) -> status` shim family: Add/Sub/Mul/...) nor in
  `step_supported` (the generic straight-line `helpers::step` list).
- They are payload-free binary stack ops exactly like `Add`, so the natural
  shape is a dedicated `helpers::` shim each, mirroring `helpers::add`
  (`src/vm/vm_jit_helpers.rs`) over the corresponding `exec_one` arms
  (`src/vm/vm_exec_dispatch.rs:2208` for `BitShiftLeft`).
- Note the bailout histogram records only the FIRST unsupported opcode per
  chunk. After adding the shifts, re-run the repro — the next blocker (if
  any) surfaces then. Expect the `+&`/`+|`/`+^` ops and possibly more; keep
  iterating until `compiles > 0` and `entries` tracks the block count.

## Why the leverage is large

Per-block cost is ~17ms interpreted vs raku's ~3ms; the 80-round reduce is
pure int bit-twiddling (and/or/xor/shift/add on native ints), the exact shape
Tier A compiles to zero-GC zero-refcount native loops (ADR-0001 §3-8,
ADR-0004). One opcode family unlocks the whole loop.

No ADR gate applies: the GC → JIT phase ordering of ADR-0001 is fulfilled
(§7 outcome table — JIT default on since 2026-07-13, ADR-0004 closed); this
is ordinary Tier A coverage work.

## Current numbers (2026-08-05, for before/after comparison)

- `rmd160("a" x 100_000)`: 26.3s
- full `tmp/libdigest-clone/t/ripemd.t`: 295.3s (9/9 pass; budget 120s)
- spawn-overhead bench `for ^2000 { await map -> $k { start { $k * 2 } }, 1, 2 }`:
  0.19s (below raku's 0.33s) — the clone-slimming campaign
  (docs/per-task-clone-slimming.md, slices 0-6) is done; do not look for
  more wins there.
