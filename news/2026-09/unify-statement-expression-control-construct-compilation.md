# One compilation pass for `for` and `if`, in both statement and expression position

Raku control constructs can be written as a statement (`for @xs { ... }`) or as
an expression that produces a value (`my @r = do for @xs { ... }`). mutsu
compiled those two positions through two independent code paths: the
`Stmt::For` / `Stmt::If` arms of `compile_stmt` (`src/compiler/stmt.rs`) and the
`compile_do_*` family in `src/compiler/helpers_do_expr.rs`. The two were not two
views of one lowering — they were two implementations of it, and every new
control-flow semantic had to be written twice.

The clearest evidence was `ForLoopSpec` (`src/opcode.rs`), the 30-field struct
that carries a compiled `for` loop's parameters to `OpCode::ForLoop`. It was
built by hand in two places, once per path, with nothing enforcing that the two
literals agreed. They did not.

## What changed

Two new modules hold one lowering each, and the position-specific callers became
thin wrappers over them:

- **`src/compiler/control_for.rs`** — `Compiler::compile_for_construct`, taking
  a `ForParts` describing the loop. `ForParts::collect` is the *only* thing the
  two positions differ in: whether each iteration's value is gathered and left on
  the stack. `ForLoopSpec` is now constructed in exactly one place in the whole
  compiler.
- **`src/compiler/control_if.rs`** — `Compiler::compile_if_construct`, taking an
  `IfPosition`. Statement and value position share the condition handling, the
  duplicated-condition value that feeds `@_` and placeholder binding, the
  jump/patch structure, the pointy-topic scope and the `elsif` recursion; they
  differ only in how a branch body is emitted and whether the chain leaves a
  value behind.

`Stmt::For` keeps its statement-level source desugars (`for @a[*]`, element-source
writeback) and `Stmt::If` keeps its statement-level pre-checks (heredoc-scope
errors, constant-condition branch resolution) — those rewrite or replace the whole
construct and re-enter `compile_stmt`, so they are not part of the lowering.

`compile_do_while_expr`, `compile_do_loop_expr` and `compile_lazy_for_expr` were
already thin: they desugar to `gather { ... take ... }` and run through the
statement path. They needed no change. `helpers_do_expr.rs` shrank from 666 to
398 lines and `stmt.rs` from 5191 to 4740.

## Divergences this fixed

Merging the two paths meant auditing every field and every step for what each
copy actually did. The value-position copy turned out to be behind on six
things, all of which the statement copy had grown at some point and the value
copy had not:

- **Loop phasers were dropped.** `compile_do_for_expr` called
  `expand_loop_phasers` and discarded its pre- and post-statement lists, so
  `FIRST` and `LAST` in a value-position `for` never fired at all.
- **A `.reverse`d rw source wrote back mirrored.** The value path always emitted
  `TagContainerRef`; only the statement path checked
  `for_iterable_is_reversed` and emitted `TagContainerRefReversed`. So
  `do for @a.reverse <-> $x { $x = ... }` wrote each element back to the wrong
  index.
- **A typed loop parameter did not autothread junctions.** The value path
  hardcoded `autothread_junctions: false`.
- **The loop parameter was not recorded as this code's own declaration**
  (`for_loop_param_syms`), so a closure in a value-position loop body could have
  its read of the parameter rewritten to a `GetUpvalue` against a same-named
  outer lexical.
- **Multi-params were not marked read-only**, and were not recorded in
  `param_bind_names` for `use strict`.
- **No block callable was built for `&?BLOCK`** — `uses_block_magic` was dropped
  at the value-position call site, so `ForLoopSpec::block_callable_local` was
  always `None` there. It is now populated in both positions, which makes
  `&?BLOCK` in a value-position `for` behave exactly like the statement form.
  (Both still report `.signature.arity` 0 where raku reports 1 — that gap is in
  how the callable is synthesized, is shared by both positions, and is untouched
  here.)

One more, on the `if` side: the value path compiled its condition with
`compile_expr` rather than `compile_condition_expr`, so a bare regex condition
(`do if /b/ { ... }`) was compiled as an always-true `Regex` literal instead of
smartmatching the topic.

`t/control-construct-value-position.t` pins the directly observable ones — the
loop phasers, the `.reverse` writeback, the read-only multi-params and the
regex condition. The junction-autothreading and `for_loop_param_syms` fields are
covered by the existing statement-form pins now that there is only one call
site building them.

## What it did not fix

Two pre-existing value-position bugs surfaced during the audit and are recorded
rather than fixed here, because neither is part of the lowering:

- `todo/tickets/next-phaser-clobbers-value-position-for-result.md` — a `NEXT`
  phaser is appended to the end of the loop body by `expand_loop_phasers`, so in
  expression position its value is what gets collected.
- `todo/tickets/nested-block-state-not-reset-in-value-position-for.md` — a nested
  bare block's `state` does not restart per iteration inside a value-position
  `for`. The unification removed one of the two causes (the value path used to
  suppress the block's `ResetStateLocals` unconditionally for a sole block,
  rather than only for the statement-modifier form the suppression exists for);
  the second cause is elsewhere and is described in the ticket.

The bare `{ ... }` block is the one construct still compiled by two independent
passes; unlike `for` and `if` its two forms emit *different* opcodes
(`BlockScope` vs `DoBlockExpr`), so unifying it is a separate, larger question —
`todo/deep/unify-block-statement-and-value-compilation.md`.
