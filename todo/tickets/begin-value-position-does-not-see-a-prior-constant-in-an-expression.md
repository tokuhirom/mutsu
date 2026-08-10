# `BEGIN <expr>` in value position does not resolve a prior `constant` used inside an arithmetic expression

Found while writing a regression test for
`todo/tickets/constant-declared-from-a-begin-is-rejected.md`. Pre-existing on
`main`, but masked there by that ticket's "Cannot assign to a readonly
variable" bug (which fired first); after that fix, this narrower bug is what
actually surfaces.

## Repro

```raku
constant A = 3;
say (BEGIN A + 1);   # raku: 4   mutsu: dies —
                      # Cannot convert string to number: base-10 number must
                      # begin with valid digits or '.' in '⏏A'
```

`A` inside the `BEGIN`'s expression is being read as the bareword string
`"A"` (then coerced numerically for `+`), not as constant `A`'s inlined
value `3`.

Narrower forms that DO work, ruling out a total resolution failure:

```raku
constant A = 3;
constant B = A + 1;      # works: B is 4 (ordinary constant-referencing-constant)
BEGIN { say A };         # works: prints 3 (statement-form BEGIN, not value position)
```

So the bug is specific to a bareword constant read occurring inside a value-
position `BEGIN <expr>` (`Expr::PhaserExpr`, compiled via
`Compiler::compile_expr_phaser`'s `PhaserKind::Begin` arm in
`src/compiler/expr_data.rs`, which wraps the body in `OpCode::BeginOnceExpr`
via `compile_block_inline`).

## Where to look

Compare how a bareword constant name resolves in the ordinary compile path
(`Compiler::constant_value` / `note_constant_decl` in
`src/compiler/const_fold.rs`, consulted during normal `compile_expr`) against
what happens inside `compile_block_inline(body)` when called from
`compile_expr_phaser`'s `PhaserKind::Begin` arm. The two `BEGIN` code paths
(statement-form via `compile_check_phaser`/reordering machinery, vs.
value-form via `compile_expr_phaser`) may thread the constant-folding table
differently, or the bareword lookup for a constant name may take a different
runtime path once inside a `BeginOnceExpr`-wrapped block versus the mainline.

## Scope

Not required by `todo/tickets/constant-declared-from-a-begin-is-rejected.md`
(that ticket's repro cases use only literal BEGIN values, not
constant-referencing expressions). Filed separately; low urgency (no known
roast/Cro blocker), but silently wrong for ordinary Raku code combining
`constant` and value-position `BEGIN`.
