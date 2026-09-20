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

## The gate is missed by a factor of 24, and the reason is the interesting part

Stage 2 IS faster. It is nowhere near the gate.

Release build, warm precomp cache, medians of 7 runs each, with rakudo taken
in the same session (the perf skill's rule for any figure that leaves this
box):

| | `from-json`, 727 records | gate |
|---|---:|---|
| `MUTSU_TRIR=off` | 1.3834 s | — |
| TRIR on | **1.0820 s** | ≤ 0.045 s — **missed**, at 38x rakudo |
| rakudo, same session | 0.0283 s | — |

A real 1.28x, and the gate asked for ~24x more.

Where it came from is the part that matters. Instruction counts are exact
where wall clock is noisy, so: callgrind, differencing two runs that differ
only in how many times `from-json` runs (1 vs 10) so that startup and module
compilation cancel.

| | marginal Ir per decode (200 records) |
|---|---:|
| `MUTSU_TRIR=off` | 2,715,806,457 |
| TRIR on | 2,456,666,797 |

**9.5% fewer instructions for 22% less time** — so the instructions TRIR
removed cost more than an average one, and they are not the ones the ADR
predicted. Of the 9.5%, 8.2 points are `memcpy` (6.09% → 1.65%) and
`core::str::count::do_count_chars` (3.02% → ~0): re-walking the document's
UTF-8, which the per-frame codepoint memo avoids. That is cache traffic over a
191 KB string, which is why it is worth more in wall clock than in issue
slots, and it is a string cache — not the typed opcodes the ADR is about.

The measurement that settles it: with TRIR **off**, the interpreter's own
dispatch loop (`exec_one` plus `exec_one_dispatch`, self cost) is **4.6% of the
decode**. ADR-0110 §1.3 divided wall clock by opcode count, got ~211 ns per
opcode, and attributed that quotient to untyped dispatch. But an opcode's cost
is overwhelmingly what its *handler* does — allocate, hash a name, intern a
symbol, copy a string — and TRIR removes the dispatch, not the handler. So
inside `run_trir_chunk`, 96% of the time is in calls back out of it, and TRIR's
own loop is about 1.2% of the program.

TRIR made the interpretation of `JSON::Fast`'s control flow nearly free, and
the decode got 1.28x faster — most of it from a string cache that came along
for the ride. The control flow was never the cost, so removing it could not be
the 24x.

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
25 KB to 210 KB, mutsu decodes at a flat 6.4-7.4 µs/byte and rakudo at
0.10-0.18 µs/byte — the ratio does not move over an eightfold change in
size, so there is no super-linear term to find and remove.

The three biggest of those are filed on their own so they survive whatever is
decided about ADR-0110 itself:
[#8898](https://github.com/tokuhirom/mutsu/issues/8898) for the allocation
traffic, [#8899](https://github.com/tokuhirom/mutsu/issues/8899) for the
run-time name resolution, and
[#8900](https://github.com/tokuhirom/mutsu/issues/8900) for the `nqp::` op
bodies and the general binder.

## Four real bugs fixed on the way

Each of these has the same tell, and it is worth naming because it makes every
TRIR bug look intermittent: a call site is linked to TRIR only once it has
executed, so the **first** call to a routine runs untyped and every call after
it runs typed. A broken TRIR path therefore produces a right answer followed
by wrong ones, which reads like flakiness and is not. Every regression test
added here calls its routine at least twice.

**A trailing `if` returned `Nil`.** In Raku a routine's final
`if`/`elsif`/`else` *is* its value. TRIR compiled the branches for effect and
emitted `ReturnNil`, so `sub sel($a, $b) { if $a && $b { 'both' } elsif ... }`
answered `both` on its first call and `Nil` for ever after. A wrong answer, not
a missing optimization — such a routine now declines.

**A generic call resolved names in the wrong package.** A TRIR frame is not a
`RoutineFrame`, so nothing set `current_package` to the routine's own
declaring package the way an untyped call does. A body declared inside
`module C` resolved its callees against the *caller's* package, and a
package-scoped `multi` was then not found at all ("Unknown function: mm").

**Every generic-call argument was handed over as a container.** A TRIR body
cannot see whether its callee will write an argument, so it containerized all
of them and read them back. That is not transparent: a container argument
reaches a `proto`'s `{*}` re-dispatch as itself and fails the winning
candidate's type check against its own type ("expected Str, got Str"), and
`nativecast`'s "type object as its first argument" check rejects it too. The
callee's `is rw` positionals are now looked up by name and only those
arguments are containerized — which is where an untyped call site ends up as
well, by a different route.

**Branch-arm unification shifted jump targets it had not moved.** The op-index
shift was computed from `then_kind != else_kind`. That is right when the
`then` arm is the native one (the box is *inserted* before its jump, shifting
everything after) and wrong when the `else` arm is (the box goes on the end
and shifts nothing). In the second case the caller looked for its own `Jump`
one slot too far along, did not find it, and declined the whole routine —
silently, with no recorded reason. `nqp::if(cond, die-helper(...), $pos)` is
exactly that shape, and it is the shape of every `JSON::Fast` scanner's error
check. `unify_arms` now answers the shift it actually applied.

The decline diagnostics grew alongside, because the fourth bug produced no
reason at all: `MUTSU_TRIR_WHY=1` now names the argument and the callee for a
call it cannot compile, and reports an unsupported expression's whole
rendering rather than only its variant name.

One general fix fell out: `call_function` — the by-name entry every caller
that is not a `CallFunc` opcode goes through — did not carry the three
`__mutsu_`-prefixed NativeCall helpers that the VM's own call opcode resolves
through its fallback chain, so `nativecast()` reached from a TRIR body
reported `Unknown function: __mutsu_nativecast`. It carries them now.

## Tests

`t/fixtures/trir-shapes.raku` gains the Stage 2 shapes — `is rw` passed on
through three resolved frames, recursion, a forward-referenced generic call, a
generic callee that writes its argument, a declaration inside a body, a list
literal, a dynamic read, and a `die` thrown out of a TRIR frame followed by a
successful call through the same routine (the frame stacks have to be intact
afterwards). `t/vm/codegen/adr0110-trir-differential.t` runs the whole file
twice, once with `MUTSU_TRIR=off`, and requires the two to agree exactly.
