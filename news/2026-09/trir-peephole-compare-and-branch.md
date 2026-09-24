# TRIR runs 43% fewer ops per JSON record: a peephole pass and sink-position `nqp::if`

ADR-0116 D2 made the bodies of TRIR's ops cheaper. After D2.4, re-measuring
showed that the *number* of ops was the next cost. The TRIR compiler emits a
plain stack form on purpose, so each compile rule reasons about one construct
and never about what the next construct does with its result. That left the
per-character loop of JSON::Fast's `unjsonify-string` at **22 ops**:

```
ElemsLocal(0)  JumpIfFalseI      ShiftILocal(0)  WrapI  StoreI(1)
LoadI(1)  ConstI(92)  EqI  JumpIfFalseI
LoadI(1)  ConstI(9)   EqI  JumpIfTrueKeepI  PopI
LoadI(1)  ConstI(10)  EqI  JumpIfFalseI
LoadI(1)  PushILocal(1)  PopObj  Jump
```

Most of it is a slot compared against a constant, dispatched as four ops
with three pushes and three pops on the native bank. The rest is a value
pushed by each `nqp::if` arm only for the join to drop it.

## What changed

- **`nqp::if` / `nqp::unless` / `nqp::stmts` in sink position compile their
  arms for effect.** No arm leaves a value, so there is nothing for the join
  to drop. The arms also no longer need a common kind, so a sink-position
  `nqp::if` whose arms leave an `int` and a `num` is now accepted instead of
  declined.
- **A peephole pass runs on every finished chunk** (`src/trir/peephole.rs`).
  It only ever replaces a window of consecutive ops by a shorter window with
  the same effect, and only when no jump lands inside the window. Jump
  targets are then renumbered through an old-to-new index map. The rules:
  - jumps to a `Jump` go straight to its target;
  - an `&&` / `||` keep jump whose target tests the same value again already
    knows that test's outcome. When its fall-through pops the value, the pair
    becomes one popping jump;
  - a push followed by a pop of the same bank is removed;
  - an int comparison followed by a conditional jump becomes `JumpCmp`. With
    a constant right operand it becomes `JumpCmpC`, and with a slot left
    operand as well it becomes `JumpCmpLC`, which touches no bank at all;
  - `ElemsLocal; JumpIfFalseI` becomes `JumpIfEmptyLocal`, and
    `PushILocal; PopObj` becomes `PushILocalVoid`.
- `TrOp::target_mut` is now the one list of jump-bearing ops.

The loop above is now 10 ops:

```
JumpIfEmptyLocal  ShiftILocal(0)  WrapI  StoreI(1)
JumpCmpLC(1 == 92)  JumpCmpLC(1 == 9)  JumpCmpLC(1 == 10)
LoadI(1)  PushILocalVoid(1)  Jump
```

## Measured

- TRIR ops executed per 100 SPDX records: **921,487 -> 528,771 (-43%)**.
- Instructions per 100 records (callgrind 1-vs-101-record difference,
  ADR-0116 §8, `MUTSU_TRIR_JIT=off`): **91.3 M -> 77.9 M (-14.7%)**. The
  switch loop itself went from 39.2 M to 25.8 M, which is now 33% of a
  record. It was 42.9%.
- `benchmarks/bench-json-fast-spdx.raku` section time, paired A/B, 7 warm runs
  each (release, 4-core container): `main` median 0.092 s (min 0.079 s),
  this change median 0.072 s (min 0.069 s). Rakudo takes 0.048-0.054 s on
  the same box. These are local numbers; quote the bench CI series.

## What it says about ADR-0116 D3

D3 brings native lowering back once the switch loop is more than 50% of a
record. Removing ops lowered that share, from 42.9% to 33%. The remaining
cost is now more concentrated in op bodies: the list ops (`JumpIfEmptyLocal`,
`ShiftILocal`, `PushILocalVoid`) call `with_nqp_backing_array` and decode
the NaN-box about five times per character.

## Pinned by

- `t/vm/codegen/adr0116-trir-peephole.t` runs `t/fixtures/trir-peephole.raku`:
  all six comparisons against a slot under `nqp::if` and `nqp::unless`, a
  constant too wide for the fused operand, computed operands, `||` / `&&`
  chains both as a condition and as a value, sink-position arms of different
  kinds, and the `unjsonify-string` drain loop. TRIR on, TRIR off and rakudo
  agree, every routine is accepted, and every fused op form appears in the
  chunks.
- Unit tests in `src/trir/peephole.rs` cover the fusion itself, a window
  entered mid-way (left alone), the threaded `||` chain, and dead pushes
  whose jump targets move on.
