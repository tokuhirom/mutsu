# `nqp::` native int ops have one body for the interpreter, TRIR and the JIT

`nqp::add_i`, `nqp::bitshiftl_i` and the other native `nqp::*_i` / `*_n` ops had their own
bodies in two executors: the interpreter's op tables (`runtime::nqp_pure::eval`) and TRIR's typed
ops (`trir/exec.rs`). On top of that, TRIR's lowering table and the JIT's inline whitelist each kept
their own copy of the op names.

The copies disagreed. `nqp::bitshiftl_i(1, 64)` was `i64::MIN` on the interpreter, which clamped
the shift count to 0..63. Under TRIR it was 1, because TRIR masked the count to six bits the way
MoarVM does. So the answer depended on whether the surrounding routine had been compiled. TRIR's
private `nqp::unbox_i` lowering also read `3.7e0` as 3, where the op itself answers 0.

`runtime::nqp_native` now holds the one scalar body of each op, with MoarVM's semantics. Every
executor calls it:

- the interpreter's `nqp_pure::eval`, which also fixes its shift counts;
- TRIR's `AddI` / `ShlI` / `DivI` / `ModI` / `IncI` ...;
- the JIT, which only inlines what those bodies answer identically on 48-bit operands.

TRIR's `nqp_form` and the JIT's `NqpIntOp` are now keyed on the `NqpPure` enum rather than on
names. `nqp::unbox_i` goes to the op.

To keep it that way, `make check-prims` gained a `native` rule against hand-written
`.wrapping_*` arithmetic in those layers. `t/fixtures/trir-int-ops.raku` pins the shift counts
against rakudo with TRIR both on and off.
