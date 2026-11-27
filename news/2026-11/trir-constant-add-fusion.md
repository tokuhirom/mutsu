# TRIR fuses a native add of a constant into one op

The TRIR compiler emits a plain stack form, so `$pos = nqp::add_i($pos, 1)`
through an `int $pos is rw` parameter was four ops: read the reference, push
the constant, add, write back. That is three pushes and three pops on the
native bank, and four trips through the switch loop. Every scanner in
`JSON::Fast` advances its position this way.

The peephole pass now folds the sequence. `LoadI(s); ConstI(c); AddI` (or
`SubI`, as `-c`) is `AddLocalIC`, and a following `StoreI(s)` of the same slot
makes it the in-place `AddLocalICVoid`. Through a reference, the same shapes
become `AddRefIC` and `AddRefICVoid`. A constant outside `i32` stays the stack
form. Inlining renames the new ops the way it renames the reference ops they
replace, so an inlined scanner's reference becomes the caller's own slot.

On the 100-record SPDX decode of `bench-json-fast-spdx`, the `from-json` call
drops from 61.14M to 58.98M instructions (-3.5%), and about 41,000 of its
432,000 TRIR ops disappear. Refs #8673.
