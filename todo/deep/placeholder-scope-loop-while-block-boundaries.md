# `while`/`loop`/bare-`{}` placeholder scope: three different rules, not one boundary fix

## Status (2026-08-20): designed — see ADR-0048; this file's central premise is WRONG

Re-audited against `main` @ `227e38e4f` with ~45 `raku`-vs-mutsu probes covering
every `{}`-bearing construct, not just the three below. The finding is real and
still open, but **its framing is falsified and should not be used to plan the
work.** Read
[`docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md`](../../docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md)
instead; it carries the full audit table and a five-phase plan.

Corrections to what is written below:

- **There are not "three genuinely different rules" — there is one rule with
  two columns.** Every `{}` body is a Block; per construct ask (a) may it carry
  a signature, and (b) what does the construct supply when invoking it. (a) =
  "no" is uniformly the compile-time `X::Placeholder::Block` that
  `compile_do_block_expr` already emits; (a) = "yes" with fewer arguments
  supplied than placeholders declared is uniformly the runtime "Too few
  positionals passed". The advice "do not batch-fix by pattern-matching the
  existing arms" is therefore backwards: a single classification oracle plus one
  shared bind emitter is exactly the right shape, and eleven independent patches
  to three mirrored walks would re-create the drift that caused this bug.
- **`loop {}` is not a special case.** `try {}`, `react {}`, `once {}`,
  `default {}`, every phaser (`BEGIN`/`ENTER`/`LEAVE`/`CONTROL`/`CATCH`),
  `race {}`, `module M {}`, and the statement prefixes `gather`/`supply`/
  `start`/`sink`/`lazy`/`eager` all take the identical compile-time error in
  raku, and mutsu accepts every one of them.
- **`while`'s divergence is worse than "missing a boundary".** mutsu supplies
  the *boolified* condition (`while 42 { $^c }` prints `True`, raku prints
  `42`; `until False { $^c }` prints `True`, raku prints `False`) **and**
  leaks the name into the enclosing block's signature
  (`{ while 42 { $^c } }.arity` is 1; raku's is 0).
- **The already-"correct" boundaries are only correct for one placeholder.**
  `if 42 { "$^a $^b" }` and `given 5 { "$^a $^b" }` raise an arity error in
  raku; mutsu prints `42 True` / `5 True`, because all four bind sites use
  `.find(|n| n.starts_with('^'))` and take the first placeholder only.
- **The bare-`{}` question (point 3) is settled and is *not* a separate
  architectural gap.** raku's behaviour is fully reproducible without making
  bare blocks real invoked closures: classify the body as signature-capable
  with zero arguments supplied and the shared arity check yields raku's exact
  "Too few positionals passed; expected 1 argument but got 0". See ADR-0048 D6.
- **Two extra divergences not recorded below:** `when` blocks are supplied zero
  arguments in raku (mutsu binds the topic), and `role R { $^c }` is *legal* in
  raku (the body runs at composition with `Mu`) while mutsu over-rejects it.

---

Split from (and supersedes) `todo/tickets/placeholder-scope-while-loop-not-a-boundary.md`.
That ticket's framing — "these constructs should be scope boundaries, just like
`if`/`for`/`given` already are" — undersold the actual complexity. A
2026-08-18 investigation against real `raku` shows **each construct has a
genuinely different rule**, not a single shared boundary decision:

```
$ raku -e 'while True { say $^c; last }'
True
$ raku -e 'my $i = 0; while $i++ < 3 { say $^c }'
True
True
True
$ raku -e 'loop { say $^c; last }'
===SORRY!=== Error while compiling -e
Placeholder variable '$^c' may not be used here because the surrounding
block does not take a signature.
$ raku -e '{ say $^c }'
Too few positionals passed; expected 1 argument but got 0
  in block <unit> at -e line 1
```

## The three rules

1. **`while COND { ... $^c ... }` binds `$^c` to the condition's value**,
   exactly like `if COND { ... $^a ... }` already does in mutsu
   (`compile_if_value`, `src/compiler/helpers_control_flow.rs:238`). It is
   its own placeholder scope (the placeholder does NOT become the enclosing
   block's parameter), but it is not "just a boundary" — the loop body needs
   an actual per-iteration value bind, re-evaluated each pass (the condition
   is re-checked and rebound every iteration, same Dup-and-bind shape
   `compile_if_value` uses for the branch).

2. **`loop { ... $^c ... }` (no condition expression) is a compile-time
   error**, `X::Placeholder::Block` — "Placeholder variable '$^c' may not be
   used here because the surrounding block does not take a signature." This
   is NOT a new error shape: mutsu already has this exact mechanism for
   `do {}` blocks (`compile_do_block_expr`,
   `src/compiler/helpers_do_expr.rs:4`, built on
   `crate::ast::collect_unattached_placeholders` +
   `method_signature_shared::placeholder_scope_error("block", ph)`, added in
   the method-`@_`-rejection work — see
   `news/2026-08/method-direct-at-underscore-should-be-rejected.md`). `loop
   {}` needs the same detect-and-die wiring, not new diagnostic
   infrastructure.

3. **A bare `{ ... }` statement is a genuinely separate invokable closure in
   raku**, not an inlined scope: `{ say $^c }` at statement/mainline position
   fails with "Too few positionals passed; expected 1 argument but got 0" —
   raku actually tries to CALL it with zero arguments and its placeholder
   makes it arity-1. mutsu's `Stmt::Block`/`Stmt::SyntheticBlock` currently
   just inlines the body into the enclosing scope
   (`collect_ph_stmt_shallow`'s `Block` arm descends unconditionally,
   `src/ast.rs:2285`), which is a structurally different compilation
   strategy, not a boundary flag. Changing this touches how bare blocks are
   compiled at all, not just placeholder attribution. **Needs its own
   from-`raku`-behavior investigation before deciding whether this part is
   in scope at all** — it may be a separate, larger pre-existing gap
   (does mutsu even treat a bare `{}` statement as an implicit call today for
   non-placeholder cases?) rather than a placeholder-specific one.

`React`, `Phaser`, `Try`, `DoBlock` (already handled - see point 2),
`PhaserExpr`/`Once`, `Catch`, `Control`, `RoleDecl` bodies (also currently
non-boundaries in `collect_ph_stmt_shallow`) were NOT individually checked
against real `raku` in this pass — each of `if`/`for`/`given`/`do{}` turned
out to have its own binding rule when audited (see the git history of
`src/ast.rs`'s `collect_ph_stmt_shallow` and
`news/2026-08/bare-precedes-placeholder-nested-scope.md`), so assume the same
is true here until checked one by one. Do not batch-fix by pattern-matching
the existing arms.

## Why this is `todo/deep`, not a `todo/tickets` slice

- It requires a NEW per-iteration value-bind codegen path for `while`
  (mirroring but not reusing `compile_if_value`'s single-evaluation shape —
  a loop condition is evaluated every pass, `if`'s is evaluated once).
- It requires auditing (individually, against real `raku`) at least 8 more
  AST constructs currently treated as non-boundaries by
  `collect_ph_stmt_shallow`/`collect_ph_expr_shallow`
  (`src/ast.rs:2204`/`2361`) and `placeholder_order.rs`'s mirroring walk
  (`check_bare_var_stmt`/`check_bare_var_expr`,
  `order_check_stmt`/`order_check_expr` after
  `news/2026-08/bare-precedes-placeholder-same-statement-order.md`) — each
  could turn out to have its own rule like `while` did here.
- Changing `collect_placeholders_shallow`'s boundary set changes the actual
  **arity** of existing blocks whose only `$^name` use sits inside one of
  these bodies today — a real, not just diagnostic, behavior change. Every
  call site of `collect_placeholders_shallow`
  (`compiler/expr_closure.rs`, `compiler/stmt.rs`) needs re-auditing for
  what it currently assumes about a block's own signature.
- The bare-`{}`-block case (point 3) may not even be a placeholder-scope bug
  at all — it may require first establishing whether mutsu compiles a bare
  `{}` statement as a real invoked closure the way raku does, which is a
  separate, likely pre-existing architectural gap.

## Severity

Low, same as the parent ticket: missing compile-time diagnostics / wrong
arity in the `while`-with-placeholder case, not observed to cause a
miscompilation of value flow (once bound, values resolve sensibly). No roast
test currently depends on any of the three rules above.

Affected: `src/ast.rs` (`collect_placeholders_shallow`,
`collect_ph_stmt_shallow`, `collect_ph_expr_shallow`),
`src/placeholder_order.rs` (mirrors the same boundaries),
`src/compiler/helpers_control_flow.rs` (`compile_if_value` is the closest
existing precedent for the `while` per-value-bind shape),
`src/compiler/helpers_do_expr.rs` (`compile_do_block_expr` is the existing
precedent for the `loop {}` rejection shape), `src/compiler/stmt.rs:2120`
(`Stmt::While` codegen) / `src/compiler/expr_block.rs:642`.

## Re-verified 2026-09-01 (TRIAGE regeneration): only `while`/`until` remain

ADR-0048 P1-P3 landed. Of the eight probes: `loop { $^c }` is rejected at
compile time, `{ say $^c }` raises "Too few positionals", `if 42 { "$^a $^b" }`
and `given 5 { ... }` raise the 2-vs-1 arity error, and `when 5 { $^c }` gets
zero arguments — all matching raku. Still diverging: `while 42 { say $^c;
last }` prints `True` (raku `42`), `until False { ... }` prints `True` (raku
`False`), and `{ while 42 { $^c } }.arity` is `1` (raku `0`) — the
condition-bind and arity-leak halves, which are ADR-0048 P4-P5 ("not started"
per the ADR header).
