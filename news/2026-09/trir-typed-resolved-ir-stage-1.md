# A statically typed routine stops re-discovering what the compiler knew

[ADR-0110](../../docs/adr/0110-typed-resolved-ir-for-statically-typed-routines.md)
measured mutsu's ~70x gap against rakudo on `JSON::Fast` and found it was not a
hot spot and not an operation count. One `from-json` of a 727-record document
executes about 4.93 M opcodes — a reasonable number — at **~211 ns each**, where
rakudo spends about 3 ns per equivalent operation. The whole of the gap is that
mutsu's bytecode is untyped and name-based, so every instruction re-establishes
at run time what the compiler already knew: a declared type travels as the
*string* `"int"`, a free variable is looked up by *name* on every access, and a
callee is resolved by *name* on every call.

Stage 1 of that ADR has landed: a routine whose variables, types and operand
kinds the compiler can prove is compiled a second time, from its own AST, into a
typed, resolved instruction set (TRIR) executed by the same VM.

## What the scanner loop looks like now

ADR-0110 §1.4's own example is `JSON::Fast`'s whitespace scanner:

```raku
my sub nom-ws(str $text, int $pos is rw --> Nil) {
    nqp::while(nqp::atpos_i($ws, nqp::ordat($text, $pos)), ++$pos);
}
```

Its body used to be `GetGlobal("ws")` — a by-name lookup, per loop iteration —
followed by two `GetLocal`s and two boxed `NqpOp`s. Its call site
`nom-ws($text, $pos)` was seven opcodes: a `GetLocal`, a `ContainerizePair` and
a `WrapVarRef` for *each* argument (including the one that is not `is rw`,
because the caller did not know the callee), then a by-name `CallFunc` through
the general binder.

The body is now six typed instructions, none of which names anything:

```
LoadI(0)  OrdAtLocal(0)  AtPosIOuter(0)  JumpIfFalseI  IncIVoid(0)  Jump
```

and the call site is one instruction, `CallTrir`, which reads both arguments
straight out of the caller's own frame slots and writes the `is rw` result
straight back.

## Measured

Release build, warm precompilation cache, the micro-benchmark from ADR-0110 §8
with its loop skeleton subtracted. Medians of 7 runs each, taken in one
session. `MUTSU_TRIR=off` forces every routine to decline, so it is the A/B
control as well as the differential-test switch. Rakudo 2026.07 was run on the
same box, in the same session: this box is slower than the one the ADR's own
figures came from, so an absolute nanosecond count from one says nothing about
the other and the ratio is the comparison that carries.

| | TRIR off | TRIR on | rakudo | speedup | vs rakudo |
|---|---:|---:|---:|---:|---:|
| `nom-ws`-shaped call, reading a file-scoped `my $ws` | 9,025 ns | **310 ns** | 237 ns | 29x | 1.31x |
| the same call with no free variable | 2,104 ns | **297 ns** | 250 ns | 7.1x | 1.19x |
| one `nqp::while` iteration | 812 ns | **17.7 ns** | 14.2 ns | 46x | 1.25x |

**The iteration gate is met and the call gate is not.** ADR-0110 §7 asks for
≤ 30 ns per iteration and ≤ 150 ns per call, as absolutes on a box whose
rakudo measured 21 ns and 204 ns on the same two benchmarks — i.e. 1.43x and
0.74x rakudo. The iteration lands at 1.25x, inside the gate either way. The
call lands at 1.31x, which is comfortably inside the ADR's kill criterion
(≤ 300 ns there = 1.47x rakudo) and just as comfortably outside the gate,
which asked for mutsu to be *faster* than rakudo on this call shape.

So Stage 1 is not a falsification — the structural claim held, and 29x on a
benchmark that eight previous slices moved by 0% is the evidence for it — but
the last ~1.8x the gate wanted is still there, and it is not where the ADR
expected. It is not in the typed body: the loop is 46x faster and the whole
of a `nom-ws` call's own instructions is a small part of what remains. It is
in the fixed per-call cost that survived — the chunk lookup and its
fingerprint check, seeding and tearing down the frame, and reading the `is rw`
argument through its container and writing it back. Those are Stage 2's to
remove, because Stage 2 has to build a real TRIR frame anyway once a TRIR body
may call another (a native `is rw` parameter then has to be a slot reference
rather than a copy in and out).

`JSON::Fast` itself is unchanged so far, and deliberately: every one of its
routines still declines, `nom-ws` included, because the real one ends by calling
`nom-comment($text, ++$pos)` and Stage 1 admits no call inside a TRIR body at
all. Making them eligible is Stage 2's whole subject.

## Two banks, so a raw word is never read as a value

ADR-0110 §3.2 describes native operands as raw words sharing the untyped operand
stack, with a debug-build stack-kind verifier as the soundness gate. This
implementation keeps the typing and drops the sharing: native `int`/`num`
operands live in their own `Vec<i64>` bank, boxed ones in the interpreter's
existing `Value` frame and stack. Nothing ever reads a raw word as a `Value`,
because no raw word is ever stored where a `Value` lives — the top risk in the
ADR's own table is removed structurally rather than contained by a check. The
collector needs no change either (the int bank holds no references) and neither
does frame teardown. The deviation is recorded in ADR-0110's implementation
status.

## The differential test earned its place immediately

ADR-0110 §5 asks for a `t/` file that runs every signature shape through both
paths and requires identical results and identical exception types.
`t/vm/codegen/adr0110-trir-differential.t` runs `t/fixtures/trir-shapes.raku` twice —
once normally, once with `MUTSU_TRIR=off` — and requires the two transcripts to
be byte-identical.

Its first run disagreed on one line, and the bug was the interesting kind. A
closure over a variable that is then passed to an `is rw` parameter kept
reporting the *pre-call* value. The cause was not in the typed code at all: by
replacing the `WrapVarRef`/`CallFunc` sequence with one opcode, `CallTrir` had
stopped carrying the call's argument-source table, and the post-compile analysis
reads exactly that table to learn that a local reaches a call and may be written
back through it. Without the signal the analysis vouched for the variable as
by-value-capturable, and the closure got a snapshot. `CallTrir` now carries the
same table the `CallFunc` it replaces would have.

That is the general shape of the risk in replacing a sequence of opcodes with
one: the opcodes were inputs to compile-time analyses as well as instructions.
