# A `with`/`without` statement modifier no longer shadows its own statement's placeholders

```raku
sub w1 { "a=$^a topic=$_" with $^n }
say w1(3, 4);   # raku: a=3 topic=4   mutsu (before): a=True topic=3
```

`with EXPR` as a statement modifier desugars to `given EXPR { if $_.defined
{ STMT } }` (`parser::stmt::modifier`), with both the synthetic `Given` and
`If` tagged `is_statement_modifier: true`. That tag was already set correctly
at parse time, but the tag has (at least) four independent consumer call
sites across the compiler — one per compilation context a placeholder-bearing
block/statement can appear in — and only ONE of them actually checked it.

## The four twins

Each of these computes "does this body have its own `$^name` placeholder
that should bind to the condition/topic value" — the normal rule for a real
`if COND { $^a }` / `given TOPIC { $^a }` BLOCK — but must skip that binding
entirely for a statement MODIFIER, whose body runs in the enclosing scope
and whose placeholders belong to the enclosing routine:

1. `stmt.rs`'s statement-position `Given` arm — **already had** the
   `!is_statement_modifier` guard (this is the "given form... already fixed"
   a prior investigation referenced).
2. `helpers_control_flow.rs`'s `compile_if_value` (statement-position `If`) —
   had `is_statement_modifier` as a parameter but never checked it for this
   purpose.
3. `helpers_do_expr.rs`'s `compile_do_if_expr_bound` (`do if`/`DoStmt`-`If`) —
   same gap.
4. `expr_block.rs`'s `DoStmt`-`Given` arm — same gap, and didn't even
   destructure `is_statement_modifier` from the AST node.

Additionally, `ast.rs`'s `collect_ph_stmt_shallow` (the shallow placeholder
collector used to decide which placeholders belong to a block's OWN
signature) had a `Stmt::If` arm that never consulted `is_statement_modifier`
either — unlike its `Stmt::For`/`Stmt::Given` siblings, which already
correctly descend into a modifier's body to expose its placeholders to the
enclosing scope. Since `with`/`without` desugar through this synthetic `If`,
this collector needed the same fix as the four compile-time sites above.

## Fix

All five sites now check `is_statement_modifier` (or, for `collect_ph_stmt_shallow`,
descend into the modifier's body) exactly the way the one already-correct
`Given` arm did. Verified against real `raku` for `with`, `without`, and
their plain `if`/`unless` statement-modifier siblings (which happened to
already work by coincidence — they don't route through the `Given`-wrapping
desugar — but are now covered by the same fix and pinned as regression
guards), plus the real-block (non-modifier) `if`/`given` cases that must
keep binding their own placeholder to the condition/topic value unchanged.

Regression tests: `t/with-statement-modifier-placeholder-scope.t`.
