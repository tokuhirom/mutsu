# TRIR's per-character `shift_i` / `push_i` reach the list storage directly

Re-measuring ADR-0116 §3.3's per-record table on `main` after the peephole
pass (#9264) put one row well ahead of the others. JSON::Fast's
`unjsonify-string` moves every character of an escaped string from one `Uni`
to another with `nqp::shift_i` and `nqp::push_i`, and those two ops cost
**~210 and ~270 instructions per character**: 13 M of the 77.9 M
instructions that 100 SPDX records take, or 17% of the decode.

Almost none of that was the edit itself. A `push_i` asked first whether the
target was a Buf (a view decode and an attribute probe), then went through
`nqp_with_elems_mut`, then through `with_nqp_backing_array` and its closure,
decoding the `Uni` and then its codepoint list, before it reached
`Vec::push`. A `shift_i` took the same layers through `nqp_shift_elem` and
`nqp_shift_int`.

## What changed

- `nqp_backing::with_list_data_mut` hands `f` the `ArrayData` of a plain list
  or of a `Uni`'s codepoint list: one view decode per level, no Buf probe, no
  closure layers. Anything else answers `None`.
- TRIR's slot-direct `ShiftILocal`, `PushILocal` and `PushILocalVoid` try it
  first and fall back to the general path when it answers `None`. The edit is
  the one the general path makes: the same node and the same `ArrayData`
  method (`shift_front`, `items_mut().push`), so nothing observable changes.
  An `IterationBuffer` or a Buf still takes the general path.

The same shortcut for `nqp::elems` (`ElemsLocal`, `JumpIfEmptyLocal`) was
measured and left out: that path was already cheap, and it moved nothing.

## Measured

Callgrind, 1-vs-101-record difference on a pre-generated document,
`MUTSU_TRIR_JIT=off` (ADR-0116 §8): **77.84 M to 71.55 M instructions per
100 records (-8.1%)**. Wall clock on the 4-core container is too noisy to
show an 8% change in single runs. Quote the bench CI series.

## Pinned by

`t/vm/codegen/adr0112-trir-list-ops.t` gains `via-buf`, which runs all
three slot-direct forms on a `buf8` parameter, the one backing the shortcut
has to hand on to the general path. TRIR on, TRIR off and rakudo agree, and
the routine is accepted into TRIR.
