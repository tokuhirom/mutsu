# nqp int ops become one instruction in the JIT

`nqp::add_i` exists to BE a single machine instruction. In a JIT-compiled loop
mutsu was spending **~654 instructions per iteration** on it. Giving the binary
`nqp::*_i` ops the inline Tier B treatment `OpCode::Add` has had since ADR-0004
J4 takes a `nqp::islt_i`/`nqp::add_i` loop from **2,699 to 1,428 instructions
per iteration**.

## What was happening

`OpCode::NqpOp` is on the `step_supported` whitelist, so a chunk containing one
still compiles — but the op itself lowers to a `helpers::step` call, which
re-enters `exec_one` and runs the whole `exec_nqp_op` protocol: a `Vec` drain
into a scratch buffer, a `VarRef` unwrap and a `Proxy` probe per operand, a
dispatch through the op registry, and a push of the boxed result. All of that to
add two integers.

The comparison is stark against the sibling opcode. `OpCode::Add` — which has
*more* to worry about, since a user can declare `infix:<+>` — gets an inline
NaN-box path with a real `iadd`. `nqp::add_i`, which nothing can override and
which is native-int by definition, got a function call.

## What it does now

`NqpIntOp::from_name` is a whitelist of twelve ops — `add_i`, `sub_i`, `mul_i`,
`bitand_i`, `bitor_i`, `bitxor_i` and the six `is*_i` comparisons — resolved at
**JIT-compile time**, not per execution. That is possible because `OpCode::NqpOp`
carries a dense registry index rather than a callee string: which op a site means
was already settled when the bytecode was compiled.

The emitted path is the `emit_int_num_arith` shape with the parts these ops do
not need removed. No `no_user_infix` guard (`nqp::` is a reserved namespace, so
there is nothing to override) and no Num path (these are native-int ops). What
remains is: check both stack words are small-Int pages, sign-extend the 48-bit
payloads, do the `iadd`/`icmp`/`band`, check the result still fits 48 bits, pack
and store. Everything else — a boxed Int, `BigInt`, `Num`, `Str`, a
`ContainerRef`, a `Proxy` — falls to the unchanged `helpers::step` shim, which
re-runs `exec_nqp_op` on the still-untouched stack.

That fallback is what keeps the semantics exactly as they were, rather than
approximately. `iarg` coerces a non-Int operand through `to_int`; `add_i` wraps
at 64 bits where the inline path can only produce a 48-bit result. The inline
path never sees a case where the plain i64 op and the interpreter arm differ,
because every such case fails the guard.

The whitelist is deliberately not a family rule. `div_i`/`mod_i` are absent
because they are *floor* division with a zero check, `bitshift*_i` because they
clamp their shift count, and the `_I` family because it is arbitrary-precision.

### The one side effect that had to come along

`exec_nqp_op` sets `Interpreter::test_pending_callsite_line` to `None` on every
op — a leftover from when nqp ops went through the generic call path, and read
by the Test module's assertion-line reporting. An inline path that skipped it
would leave a stale line behind for whatever reads it next.

So the inline path does it too, with one store. `Option<i64>`'s layout is not
guaranteed by Rust, so the discriminant word is found by the same probe
discipline `probe_vec_layout` already uses in that module — match by value
rather than assume a field order — with one addition: the probe **performs the
write it is licensing** and checks the result is `None`. "Which word is the tag"
and "does storing zero there mean `None`" are two different facts, and only the
second is what the emitted code depends on. If the probe does not resolve, only
this path is disabled, not the rest of Tier B.

## Measurements

Baseline `a49fc918`. `--profile profiling` builds of the same tree with and
without the change; the first run after each build discarded.

| | before | after | |
| --- | ---: | ---: | ---: |
| `nqp::islt_i` + `nqp::add_i` loop | 283,928,345 | **157,755,246** | **-44.4%** |
| plain `$j = $j + 1` loop | 225,113,779 | 225,006,170 | -0.05% |
| `bench_json.raku`, 100 records | 1,845,278,796 | 1,843,258,057 | -0.11% |

Subtracting the ~15.0M startup, the nqp loop goes **2,699 -> 1,428 instructions
per iteration**. The plain loop contains no nqp op and the JSON parse barely
uses them, which is exactly why they do not move: this is a targeted win, not a
general one.

In wall clock, on a release build and a 100-million-iteration loop — long
enough that subtracting process startup is not the measurement:

| | total | startup | per iteration |
| --- | ---: | ---: | ---: |
| mutsu | 10.036s | 0.008s | **100.3 ns** |
| `raku` | 0.434s | 0.218s | **2.16 ns** |

So the loop is **~46x rakudo**, from ~87x before this change. rakudo's 2.16 ns
is a JIT-compiled native register loop — roughly five cycles — which is what
the register-residency work below has to aim at.

## Correctness

Output is byte-identical with `MUTSU_JIT` on and off across every boundary the
guard exists for: the small-Int ceiling and floor, i64 wrapping, `Str` and `Rat`
operands, a bound container operand, signed comparisons, and bit ops across the
sign boundary. Every numeric case also matches `raku` exactly.

The one divergence from rakudo is pre-existing and untouched: `nqp::add_i("41", 1)`
throws "This type cannot unbox to a native integer" under rakudo and returns 42
under mutsu, because `iarg` coerces. The inline path declines on that operand
shape, so it answers 42 exactly as the interpreter always did.

Pinned by `t/vm/nqp-int-ops-jit-inline.t` (22 assertions), which passes with the
JIT on and off.

## What is left

The loop is now 1,428 instructions per iteration against rakudo's ~11 cycles.
The remaining structure, from the same profile, is interpreter dispatch,
`try_enter_range`'s per-iteration JIT entry probe, and `Env::get_sym` in a loop
that should be pure slot access — but underneath all of them sits the constraint
`vm_jit_tier_b.rs`'s own module doc states: the stack data pointer and length are
**reloaded at each opcode**, so every intermediate value round-trips through
memory even when Tier B is emitting real `iadd`. Keeping values in Cranelift SSA
values across a basic block, with the VM stack materialized only at block
boundaries and on deopt, is the next structural step and wants its own ADR.
