# Native-int arithmetic and string comparison each have one home

Three follow-ups to the nqp-alignment review (#9455). Each one leaves a
primitive with a single implementation (ADR-0117 / ADR-0118).

**`OpCode::NativeIntArithmetic`** computes `+`, `-` and `*` on
compiler-proven native integers. It used to hand-write
`left.wrapping_add(right)` and similar, alongside `runtime::nqp_native::add_i`,
which the `nqp::` op tables, TRIR and the JIT already share. Today the two
agree. The opcode had only escaped `make check-prims` because the gate's native
scope stopped at the nqp layers.

- The signed arm now calls `nqp_native::{add_i, sub_i, mul_i}`.
- The unsigned arm calls the new `nqp_native::{add_u, sub_u, mul_u}`. These
  wrap modulo 2**64 and return the register's bits boxed as a signed Int, which
  is how Rakudo boxes a `uint` register.
- `scripts/check-prims.sh` now scans `src/vm/vm_arith*.rs` too, and its
  self-test covers that, so the copy cannot come back.

**`nqp::iseq_s` / `isne_s` / `cmp_s`** used to copy both operands with
`to_string_value()` before comparing. The core of every string comparison is now
in `builtins::str_prim`:

- `str_eq` serves `eq`/`ne` and `iseq_s`/`isne_s`.
- `str_order` serves `leg`/`lt`/`gt`/… and `cmp_s`.

The operators still coerce and autothread first, and the nqp ops still take the
string form as it is. The nqp ops now borrow a plain `Str` rather than copying
it, so `iseq_s` on operands of different lengths is O(1), as it is in MoarVM.
`t/vm/nqp-str-prim-parity.t` gains eight cases that pin each op against its
operator.

**`nqp_op_registry_names_all_dispatch`** was named in the module doc of
`src/runtime/nqp_op_ids.rs` as the test that keeps the registry honest, but it
did not exist. It exists now: it dispatches every registered op by its id and
asserts that none falls through to `Unsupported nqp:: op`.
