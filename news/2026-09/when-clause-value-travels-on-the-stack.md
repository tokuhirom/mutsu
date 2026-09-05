# A `when` clause now produces its value on the VM stack

`when EXPR { BODY }` is an ordinary value-producing statement in Raku: on a
match it is the block's value and abandons the rest of the block, and on a
*non*-match "the block is not abandoned since the comparison is false"
(`raku-doc/doc/Language/control.rakudoc`) and the clause evaluates to the failed
comparison's falsy result. mutsu implemented that with three different value
transports for one construct, and the non-match answer reached only four of
them — the inlined `.map` / `.grep` / `.first` fast paths, through an
interpreter-global `Interpreter::when_nonmatch_value` field. Every other way of
invoking a when-tail block (a direct closure call, `do { when … }`, a
`given`/`when` statement, a routine whose tail is a when-chain) read the
block's compiled return value and found `Nil`/`Any`.

This implements [ADR-0052](../../docs/adr/0052-a-when-clause-produces-its-value-on-the-stack.md)
Slices 2-4 (Slice 1, the loop stack-base discipline, shipped 2026-08-23) and
retires `todo/deep/when-nonmatch-value-outside-map-grep.md`.

## What changed

**The clause pushes, on both branches.** `exec_when_op` pushes the falsy value
on a non-match instead of writing the side channel, so the ordinary value stack
is the only transport. `when_nonmatch_value` is deleted, along with
`tail_is_when_chain`, the four fast paths' `tail_is_when` plumbing, and
`OpCode::PushWhenNonmatch` — an opcode that existed solely to read the side
channel back out for the term form `do when …`.

**All eight statement-sequence compilers agree.** The `Pop` after a non-value
`given` was guarded by a `matches!(stmt, Stmt::Given { .. })` test that each
compiler spelled for itself, and only `compile_unit` had extended it to
`When`/`Default`. It is now one shared predicate,
`Compiler::stmt_nets_a_stack_value`, used by `compile_unit`, both routine-body
loops, both closure-body loops, `compile_stmts_value`,
`compile_body_with_implicit_try_inner`, `compile_block_inline`,
`compile_phaser_block_scope` and `compile_try_region` — the last three of which
had no such `Pop` at all, even for `Given`. The tail arms were widened to
match, so a tail `when` is the sequence's value rather than being buried under
`compile_block_inline`'s unconditional trailing `LoadNil`.

**The match branch no longer reads below its own stack range.** It took the
body's value with a bare `stack.last()`, which has no floor: when a matching
body's tail statement produced nothing, the clause handed out the *enclosing
frame's* stack top. `say "A: ", (given 2 { when 2 { my $x = 5 } })` printed
`A: ` — the literal `"A: "` was consumed as the clause's value. `exec_when_op`
and `exec_default_op` now own a stack base, take the value from above it and
truncate. The tail-`VarDecl` arm this exposed as missing was added to
`compile_when_tail_stmt`, so that expression is `5` as in Rakudo.

**A collecting loop keeps the matching iteration's value.** The `is_succeed`
arms of the eager `for`, both lazy `for` variants, the `while` loop and the
C-style/`repeat` loop discarded the signal's value, so
`do for 1..3 { when 2 { "hit" }; "plain" }` had two elements where Rakudo has
three. They now collect it.

**The falsy value is selected by the matcher's syntax, not by the comparison.**
ADR-0052 §2.4 proposed making `vm_smart_match` value-returning so the clause
could push "what the comparison actually produced". Measurement retired that
premise: `(Any ~~ 2)` written as an ordinary expression is `Bool::False`, while
`given Any { when 2 {…} }` is `Int 0` — same topic, same matcher, same runtime
result, different answer. Rakudo's `Int 0` is an artifact of the lowering it
picks for the matcher's *spelling*, so no runtime value can carry it.
`OpCode::When` now carries a compile-time `WhenMatcherKind`, and the whole
measured table is reproduced: a bare name that resolved to a type object
(`when Str`, `when Nil`) is `Int 0` for any topic; a literal constant
(`when 2`, `when "y"`) is `Int 0` against an undefined topic and `Bool::False`
against a defined one; and anything the source has to evaluate — a variable
even when it holds `Str`, a named `constant`, a regex, a range, a block — is
always `Bool::False`. This also fixes the one row that was wrong on the
already-"fixed" fast path: `(Any,).map({ when 2 { "x" } })` is `(0,)` now, not
`(Bool::False,)`.

**The postfix `STMT when COND` spelling is deliberately exempt.** It is not a
`when` clause — Rakudo lowers it to a plain conditional — and a false one is
`Empty`, not the clause falsy value. `exec_when_op` already knows which
spelling it compiled, so it pushes `Empty` there.
`t/when-statement-modifier.t`'s assertion was tightened from `is …, Nil` (which
only passed because both stringify to the empty string) to
`is-deeply …, Empty`.

## Pins

`t/when-clause-value-on-stack.t` — 25 assertions covering the three origin
probes, the whole matcher-kind table, the peek-below-the-base case, the
non-last-clause pop cases, the collecting-loop rows and the modifier row. All
25 pass unchanged under real `raku`, which is how each expected value was
established. `t/when-only-block-nonmatch-value.t` test 11 flipped from `todo`
to a plain assertion. The 62 whitelisted roast files containing a
statement-initial `when`/`default`/`given`/`succeed`/`proceed` all pass
(3532 assertions).
