# The big-integer `nqp::*_I` op family

Only `nqp::add_I` and `nqp::sub_I` existed; every other P6bigint op died with
`Unsupported nqp:: op` (#9344). Rat::Precise (`islt_I`), Bits (`isne_I`) and
mod-div-specs (`div_I`) all stopped on one of them.

The whole family now exists: `iseq_I isne_I islt_I isle_I isgt_I isge_I cmp_I
mul_I div_I mod_I pow_I neg_I abs_I gcd_I lcm_I bitand_I bitor_I bitxor_I
bitneg_I bitshiftl_I bitshiftr_I`, in a new `src/runtime/nqp_ops_bigint.rs`
(with `add_I`/`sub_I` moved alongside them) and registered in the `nqp::` op-id
table.

In MoarVM these ops *are* Rakudo's Int operators (`infix:<div>` on two Ints is
`nqp::div_I`), so per ADR-0118 none of them carries its own arithmetic: each is
a call into the shared Int home in `src/builtins/arith/`. That home gained the
pieces it was missing -- `int_gcd`, `int_lcm`, `int_bitneg`, `int_abs_value`
and `int_cmp` -- and the VM's `gcd`/`lcm`/prefix `+^` opcodes, which each had a
private copy, now call them too. The MoarVM specifics are kept: comparisons
answer a native 0/1, `div_I` floors and `mod_I` takes the divisor's sign, and a
negative `pow_I` exponent answers a Num (`pow_I(2, -1)` is 0.5, not the Rat
`2 ** -1`). A zero divisor raises instead of taking the process down with a
SIGFPE the way MoarVM does.

Every result was compared against rakudo 2026.07 over negative operands and
values past 64 bits. Pinned by `t/vm/nqp-bigint-op-family.t`.
