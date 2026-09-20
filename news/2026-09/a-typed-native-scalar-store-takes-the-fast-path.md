# A typed native scalar store takes the fast path

`my int $i` made every assignment to `$i` run the full store cascade. Removing
that takes a `nqp::add_i` loop from **328 to 188 ns/iter** and a plain
`$j = $j + 1` loop from **274 to 147** — 43% and 47% — and moves the plain
loop from 5.7x rakudo to **2.85x**.

This is [#8830](https://github.com/tokuhirom/mutsu/issues/8830)'s suggested-shape
item 3, and the prerequisite [#8831](https://github.com/tokuhirom/mutsu/issues/8831)
needs before any of its register work can be designed: a declared type
constraint stops being re-derived on every store.

## What was happening

`exec_set_local_scalar_fast` exists to bypass a ~2,000-line store-flavour
cascade, and it declined outright for any slot whose name could carry a type
constraint (`env_type_constraint_seen_for`). So a native-typed local — the
exact thing a numeric loop uses — never reached it.

Measured on the two loops #8831 uses, 100k iterations, startup subtracted:

| | Ir | share | per iter |
|---|---:|---:|---:|
| `exec_set_local_op_inner`, `nqp::add_i` loop | 87,301,053 | 22.0% | 873 |
| `exec_set_local_op_inner`, plain loop | 87,301,053 | 25.8% | 873 |

**Byte-identical between the two**, #1 in both, and none of it about `nqp::`:
873 instructions to store one integer into a slot. The profile shows the fast
path declining **100,001 of 100,001** stores.

## What it does now

The typed term splits out of `set_local_scalar_fast_metadata_clear` (which
`OpCode::ConcatAssignLocal` shares and still wants whole) into
`slot_type_constraint_possible`, and the fast path answers it *against the
incoming value* instead of declining.

**Nothing is assumed from the declaration.** The three steps the typed branch
runs are each checked to be a no-op for this exact (constraint, value) pair,
and anything else declines:

* `type_matches_value` — a native scalar constraint against a value already
  carrying that tag.
* `try_coerce_value_for_constraint` — returns its argument unchanged past the
  coercion arm, which a `(...)`-free name cannot enter, *provided no subset can
  redirect the name*. Hence the `subsets.is_empty()` term: a `subset int of …`
  would change what the constraint means, and then this must not fire.
* `wrap_native_int_by_constraint` — the identity for `int`/`int64` given a
  non-`BigInt`, non-`Bool` integer (its width check only rejects a `BigInt`),
  and for `str`/`num`/`num64`, which are not native *int* types and are not
  `num32` (the one width that truncates).

The narrower widths (`int8`, `uint32`, `num32`, …) are deliberately **not**
served: each wraps or truncates, so the store is not the identity and the
cascade has real work to do. `my int8 $b; $b = 300` still yields 44,
`my int $i = 3; $i = Nil` still yields 0, and both `my int $i; $i = "x"` and
`my Str $s; $s = 42` still raise the type check — all of them decline the gate
and run the unchanged cascade.

## Measurements

Release builds of the same tree with and without the change, three runs each,
first run after the build discarded.

| loop | before | after | | vs `raku` |
| --- | ---: | ---: | ---: | --- |
| `nqp::islt_i` + `nqp::add_i` | 328.4 ns/iter | **187.8** | **-42.8%** | 108x → **57x** |
| plain `$j < N` + `$j = $j + 1` | 274.3 ns/iter | **146.7** | **-46.5%** | 5.7x → **2.85x** |

Instruction counts, 100k iterations, `--profile profiling` (deterministic):

| | before | after | | per iter |
| --- | ---: | ---: | ---: | --- |
| nqp loop | 396,881,612 | 284,883,660 | **-28.2%** | 3,819 → 2,699 |
| plain loop | 338,058,547 | 226,064,174 | **-33.1%** | 3,231 → 2,111 |

`raku` on the same box is 3.3 ns/iter for the nqp loop and 51.4 for the plain
one — its `nqp::` path is native-register while its plain path is boxed, which
is why the *same* mutsu loop body reads as 57x or 2.85x depending only on what
it is compared against.

### It does nothing for `JSON::Fast`, and that is not a surprise

727-record parse: **2.107s before, 2.102s after** — no change. The reason is in
the profile rather than in a guess: the whole `exec_set_local*` family is about
**0.5%** of that run. JSON::Fast declares `my int` 36 times, but not on its hot
path.

So the shape of this win is narrow and worth stating plainly: **the typed local
store is 22-26% of a tight numeric loop and ~0.5% of a document parse.** It is
a loop win.

## What is left

The nqp loop is now 2,699 instr/iter against rakudo's ~11 cycles. What remains,
from the same profile:

* `exec_nqp_op` + `call_nqp_op`, ~654/iter — `OpCode::NqpOp` is in
  `step_supported`, so it lowers to a `helpers::step` call while `OpCode::Add`
  gets an inline tier-B path (`vm_jit_tier_b.rs`). Giving `NqpOp` the same
  treatment follows an existing template.
* Interpreter dispatch, ~380/iter.
* `try_enter_range`, the JIT entry probe, ~157/iter — once per iteration.
* `Env::get_sym`, ~80/iter, in a loop that should be pure slot access.

And underneath all of them, the constraint `vm_jit_tier_b.rs`'s own module doc
states: the stack data pointer and length are **reloaded at each opcode**
("never cached across a helper call, which may reallocate the Vec"), so every
value round-trips through memory between opcodes. That is the piece that
genuinely reaches for ADR-0001's rejected level 2, and it wants its own ADR.
