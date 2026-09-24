# The per-character copy loop of `unjsonify-string` runs 6 TRIR ops, not 10

After #9274 made each list op cheaper, the per-character loop of JSON::Fast's
`unjsonify-string` still dispatched 10 TRIR ops per ordinary character. It
was about half of every op the SPDX decode executes:

```
JumpIfEmptyLocal  ShiftILocal(0)  WrapI  StoreI(1)
JumpCmpLC(1 == 92)  JumpCmpLC(1 == 9)  JumpCmpLC(1 == 10)
LoadI(1)  PushILocalVoid(1)  Jump(header)
```

## What changed

Three more peephole rules (`src/trir/peephole.rs`):

- `ShiftILocal(l); [WrapI;] StoreI(s)` becomes `ShiftIStoreLocal`. It shifts
  from the list slot, wraps to the declared size and stores into the native
  slot, without touching the operand bank.
- `LoadI(s); PushILocalVoid(l)` becomes `PushISlotLocalVoid`. It pushes a
  native slot onto the list slot, also without touching the bank.
- **Loop rotation.** A backward `Jump(h)` onto a loop header
  `JumpIfEmptyLocal { slot, target: e }` becomes
  `JumpIfNonEmptyLocal { slot, target: h + 1 }; Jump(e)`. Every later
  iteration then runs one test instead of a jump and a test. Only backward
  jumps are rotated, so the forward `Jump(e)` left behind is never rotated
  again. The rule adds ops, so it runs once, after the other rules reach
  their fixpoint.

The loop is now:

```
ShiftIStoreLocal(0 -> 1, u32)
JumpCmpLC(1 == 92)  JumpCmpLC(1 == 9)  JumpCmpLC(1 == 10)
PushISlotLocalVoid(1 <- 1)  JumpIfNonEmptyLocal(-> body)
```

## Measured

Callgrind, 1-vs-101-record difference, decode only, `MUTSU_TRIR_JIT=off`
(ADR-0116 §8): **72.42 M to 70.76 M instructions per 100 records (-2.3%)**.

The number is small, and that is the finding. Four fewer ops per character
over ~27,000 characters is ~108 K fewer dispatches, and they saved ~15
instructions each. So dispatch plus bank traffic is cheap per op. The switch
loop's ~50 instructions per op average comes from the bodies of the heavy
ops that are inlined into it (the generic `nqp::` ops, object construction),
not from dispatch. Cutting the op count further is not where the rest of
#8673's gap is.

## Pinned by

- `t/vm/codegen/adr0116-trir-peephole.t` checks that all three new forms appear
  in the chunks. Its fixture gains `drain-u8`, the copy loop through a
  wrapping `uint8`. TRIR on, TRIR off and rakudo agree.
- Two unit tests in `peephole.rs` cover the fused and rotated copy loop, a
  shift-and-store without a wrap, and a back edge whose header is not an
  emptiness test, which is left alone.
