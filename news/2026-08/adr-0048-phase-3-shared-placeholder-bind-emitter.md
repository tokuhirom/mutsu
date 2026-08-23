# ADR-0048 Phase 3: one shared placeholder-bind emitter, and raku's arity error

`if 42 { "$^a $^b" }` printed `42 True` in mutsu. In raku it dies with
`Too few positionals passed; expected 2 arguments but got 1`, because the
branch is a Block declaring two positional parameters and `if` invokes it with
exactly one argument — the raw condition value.

The cause was structural, and is what ADR-0048 D3 was written to fix: the
value-binding half of the block-invocation contract was copy-pasted at five
codegen sites, each of which located only the *first* placeholder
(`collect_placeholders_shallow(..).find(|n| n.starts_with('^'))`) and let any
further placeholder fall through to the *enclosing* block's signature. A
boolean "descend or stop" per AST arm has nowhere to record how many arguments
a construct actually supplies, so there was nothing to compare a body's arity
against.

## What landed

A single compiler helper,
`Compiler::emit_inlined_body_placeholder_binds(body, supplied)`
(`src/compiler/helpers_placeholder_binds.rs`), now owns that half of the
contract. It collects **all** of a body's caret placeholders in
`collect_placeholders_shallow` order, binds as many as the construct's
`ArgSupply` provides, and emits raku's verbatim failure — including the
singular `expected 1 argument` — for the rest. All five sites call it:
`Stmt::If`, `compile_if_value`, `compile_do_if_expr_bound`, `Stmt::Given`, and
the value-position `do given`.

Three things fell out of unifying them:

- **`@^a`/`%^a` count.** The old `starts_with('^')` filter silently skipped the
  non-scalar placeholder forms, so `if 42 { $^a; @^b }` bound `$^a` and
  dropped `@^b`. raku reports both as positionals (`expected 2 arguments but
  got 1`), and now so does mutsu.
- **`when` and the bare `{ ... }` statement are zero-argument bodies** (D6).
  Classifying them `Signature(ArgSupply::None)` makes them boundaries the
  shallow walks stop at, and the same emitter produces
  `expected 1 argument but got 0` for `{ $^c }` and
  `given 5 { when 5 { $^c } }`. That retires both copies of the ad-hoc
  `"Implicit placeholder parameters are not available in bare nested blocks"`
  string — and fixes the *non-tail* bare block, which had no check at all and
  leaked its placeholder straight onto the enclosing routine's signature
  (`sub f { { $^c }; 99 }` gave `f` arity 1 where raku gives it `()`).
  `Stmt::SyntheticBlock` — the parser's desugar wrapper, with no `{ ... }` in
  the source — is deliberately excluded and stays transparent.
- **A missing guard surfaced.** The statement-position `Stmt::If` site never
  had the `!is_statement_modifier` guard its two value-position siblings
  carried, so a non-tail `if` *statement modifier* bound the enclosing
  routine's placeholder to the modifier's condition:
  `sub f { say "$^a" if 1; 0 }; f(7)` printed `1` instead of `7`. The tail form
  went through `compile_if_value` and was already right, which is why the bug
  had stayed invisible. One emitter, one guard, at all five sites.

D6 also turned up a shape the model had to grow a name for: a statement
modifier's *modified statement* can itself be a bare block, and then that block
is the construct's own. `{ $a = $^x } unless 0` parses to an `If` whose branch
is exactly `[Stmt::Block(inner)]` — the very same shape as the genuinely nested
`if 1 { { $^a } }` — but raku supplies the modifier's value to it (it prints
`0`; `{ $a = $^x } given 69` prints `69`) rather than invoking it with nothing.
`note_construct_body_block` records that one body block's address so
`is_construct_body_block` can let it through the zero-argument check, re-noted
by the loop arms after `expand_loop_phasers` rebuilds their body list.
`t/statement-modifiers.t` and `roast/S04-statement-modifiers/{if,unless}.t`
caught this immediately, which is the safety net working as intended.

Phase 3 also had to promote `repeat {} while/until` to its real
`Signature(ArgSupply::ConditionAfterFirstPass)` classification — the
classification half of D4, no new codegen. Once a bare `{ ... }` statement
became a zero-argument boundary, a `repeat` nested inside one leaked its `$^a`
out to that block, which then reported it as a parameter nothing supplies.
That is precisely the shape of `roast/S04-statements/repeat.t`'s
"placeholders and 'repeat while' mix" subtest, so the promotion was a
prerequisite rather than scope creep. A placeholder in a `while` body still
leaks; supplying the raw condition per iteration for all three remains D4.

## Verification

`t/placeholder-scope-signature-capable.t` adds 36 cases — one per row of
ADR-0048's signature-capable evidence table, plus the
modifier-over-a-bare-block group — and **passes unmodified under real `raku`**
as well as under mutsu, so every expectation is the rakudo observable rather
than mutsu's own output. The exact failure text is asserted through an
`EVAL`/`CATCH` helper rather than `throws-like`'s `message` matcher, which
mutsu currently accepts and ignores.

Several neighbouring divergences were measured and deliberately left to D4:
`{ @a.push($^x) } for 1, 2` yields `True` per element instead of the element
(confirmed pre-existing by temporarily reverting the classification), `while`
still leaks a placeholder in its body to the enclosing routine, and a
genuinely nested `if 1 { { $^a } }` prints `True` where raku raises. They are
recorded in the ADR so Phase 4 inherits a list rather than a surprise.
