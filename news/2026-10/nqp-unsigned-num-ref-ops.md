# nqp: unsigned, num and element-reference ops for NativeCall

The first slice of ADR-11203 (running upstream NativeCall verbatim) adds the non-FFI `nqp::` ops
that `NativeCall.rakumod` and `NativeCall/Types.rakumod` use:

- `unbox_n`: refuses an `Int`, as MoarVM does.
- `unbox_u`: wraps modulo 2**64, so `-1` reads as `2**64 - 1`.
- `atpos_u` and `bindpos_u`, for native arrays and for `Buf`s at their own width.
- `atposref_i`/`_n`/`_u`: the same element container a `:=` bind produces, so a write through it
  lands in the array.
- `setcodename`: shares one routine with `Code.set_name`.
- `neverrepossess`: a no-op, because mutsu does not serialize compiled modules.

They live in a new seventh `nqp::` table, `nqp_ops_native.rs`, chained after the list table.

Two limits remain:

- `atposref_*` into native-backed `CArray`/`Buf` storage waits for the REPR work in #11209.
- A write through a bound native-int element does not wrap yet (#11233).
