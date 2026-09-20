# TRIR Stage 2: JSON::Fast's decode runs on typed IR, and that is not where its time goes

[ADR-0110](../../docs/adr/0110-typed-resolved-ir-for-statically-typed-routines.md)
Stage 1 gave statically-provable routines a typed, resolved IR and measured a
29x speedup on a `nom-ws`-shaped call and 46x on a `nqp::while` iteration —
but it moved `JSON::Fast` by nothing at all, because a Stage 1 body could not
call anything and every one of `JSON::Fast`'s scanners ends in a call. Stage 2
removes that restriction. It is the stage whose gate was `from-json` on the
727-record document at **1.04 s → ≤ 0.045 s**.

## What Stage 2 does

A TRIR body may now call out, in two shapes that the compiler picks between by
what it can prove:

- **Resolved** (`CallTr`) when the callee already has a chunk of its own, so
  its signature is known at the call site. The binder is compiled away and a
  native `is rw` parameter is passed as the absolute index of the caller's
  slot — MoarVM's `getlexref_i`, and the only form that stays correct when the
  callee passes the parameter on again.
- **Generic** (`CallGen`) for everything else: a forward reference (`nom-ws`
  calls `nom-comment`, declared eleven lines later), a builtin, or a cold `die`
  helper whose body is arbitrary Raku. Arguments are boxed and ordinary
  dispatch takes it.

The generic form is what keeps eligibility from collapsing. Refusing a routine
for its cold error path would leave its hot loop untyped too.

Around that: frames nest in one contiguous native/boxed pair of stacks
(`src/trir/frame.rs`), with the boxed halves visited as GC roots; container
declarations, `nqp::ifnull`, list literals, string concatenation, hash and
array construction, dynamic-variable reads and branch-arm unification all
compile. In `JSON::Fast` the result is that `nom-ws`, `parse-thing`,
`parse-obj`, `parse-array`, `parse-string`, `parse-true` and `parse-false` —
the entire hot decode — run as TRIR. Only `parse-numeric` still declines on
the hot path.

## The gate is missed, and the reason is the interesting part

`from-json` on the 727-record document did not get measurably faster.

Instruction counts are exact where wall clock is noisy, so the honest number
comes from callgrind, differencing two runs that differ only in how many times
`from-json` runs (1 vs 10) so that startup and module compilation cancel:

| | marginal Ir per decode (200 records) |
|---|---:|
| `MUTSU_TRIR=off` | 2,715,806,457 |
| TRIR on | 2,456,666,797 |

**9.5% fewer instructions, and no measurable wall-clock change** — the removed
instructions are cheap, high-IPC ones. Worse, they are not the ones the ADR
predicted: of the 9.5%, 8.2 points are `memcpy` (6.09% → 1.65%) and
`core::str::count::do_count_chars` (3.02% → ~0), which is the per-frame
codepoint memo, not the typed opcodes.

The measurement that settles it: with TRIR **off**, the interpreter's own
dispatch loop (`exec_one` plus `exec_one_dispatch`, self cost) is **4.6% of the
decode**. ADR-0110 §1.3 divided wall clock by opcode count, got ~211 ns per
opcode, and attributed that quotient to untyped dispatch. But an opcode's cost
is overwhelmingly what its *handler* does — allocate, hash a name, intern a
symbol, copy a string — and TRIR removes the dispatch, not the handler. So
inside `run_trir_chunk`, 96% of the time is in calls back out of it, and TRIR's
own loop is about 1.2% of the program.

TRIR made the interpretation of `JSON::Fast`'s control flow nearly free. The
decode did not get faster, because the control flow was never the cost.

## Where the decode's time actually is

The marginal decode profile is flat — nothing above 4.2%, and a very long tail:

| | share of one decode |
|---|---:|
| the allocator (`malloc`/`free`/`realloc` and their internals) | ~11.6% |
| `memcpy` / `memcmp` / `memchr` | ~5% |
| the untyped dispatch loop | ~4.6% |
| hashing and hash-table probes | ~3.3% |
| thread-local access (the `Symbol` interner, `MetaNs`) | ~2.8% |
| NaN-box encode/decode | ~3% |
| run-time type-name resolution (`try_resolved_type_capture_name` and friends) | ~1.5% |

By inclusive cost, with TRIR on, `nqp::` op bodies are 16.9% of the decode, the
general binder 16.1%, `Env::get_sym` 9.3%, and `Symbol::intern` 4.3%.

The allocation count is the number that stands out: **1,628,921 heap allocations per
decode of 200 records** (against 1,659,471 with TRIR off), i.e. about 8,100
allocations per JSON record of seven fields and a two-element array.

And it is a constant factor, not an asymptotic bug. Across documents from
25 KB to 210 KB, mutsu decodes at a flat ~6.2 µs/byte and rakudo at
~0.15 µs/byte — 41x, unchanged by an eightfold change in size.

Each of those three costs is filed on its own so it survives whatever is
decided about ADR-0110 itself:
[#8898](https://github.com/tokuhirom/mutsu/issues/8898) for the allocation
traffic, [#8899](https://github.com/tokuhirom/mutsu/issues/8899) for the
run-time name resolution, and
[#8900](https://github.com/tokuhirom/mutsu/issues/8900) for the `nqp::` op
bodies and the general binder.

## One real bug fixed on the way

Branch-arm unification computed its op-index shift from `then_kind !=
else_kind`. That is right when the `then` arm is the native one (the box is
*inserted* before its jump, shifting everything after) and wrong when the
`else` arm is (the box goes on the end and shifts nothing). In the second case
the caller then looked for its own `Jump` one slot too far along, did not find
it, and declined the whole routine — silently, with no recorded reason.

`nqp::if(cond, die-helper(...), $pos)` is exactly that shape: a boxed `then`
against a native `else`. It is the shape of every `JSON::Fast` scanner's error
check. `unify_arms` now answers the shift it actually applied.

The decline diagnostics grew alongside: `MUTSU_TRIR_WHY=1` now names the
argument and the callee for a call it cannot compile, and reports an
unsupported expression's whole rendering rather than only its variant name.

## Tests

`t/fixtures/trir-shapes.raku` gains the Stage 2 shapes — `is rw` passed on
through three resolved frames, recursion, a forward-referenced generic call, a
generic callee that writes its argument, a declaration inside a body, a list
literal, a dynamic read, and a `die` thrown out of a TRIR frame followed by a
successful call through the same routine (the frame stacks have to be intact
afterwards). `t/vm/codegen/adr0110-trir-differential.t` runs the whole file
twice, once with `MUTSU_TRIR=off`, and requires the two to agree exactly.
