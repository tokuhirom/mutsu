# A multi-parameter pointy block as a `for` statement-modifier operand ignores its arity

`EXPR for LIST` where `EXPR` is a closure uses that closure as the loop body, so
the loop consumes as many elements per iteration as the closure's arity. mutsu
only ever supplies one.

## Minimal repro

```raku
say (-> $a, $b { "$a/$b" } for 1, 2, 3, 4);
my @r; -> $a, $b { @r.push("$a/$b") } for 1, 2, 3, 4; say @r;
```

* `raku`: `(1/2 3/4)` / `[1/2 3/4]`
* `mutsu`: `Too few positionals passed; expected 2 arguments but got 1`

Both the expression-position and the statement-position spelling fail. The
single-parameter forms (`-> $x { ... } for ...`, `sub ($x) { ... } for ...`) work,
as does a bare block with placeholders (`{ $^a ~ $^b } for (1,2),(3,4)`, fixed in
`news/2026-08/bare-block-as-statement-modifier-operand.md`).

## Root cause

`parse_single_modifier`'s `for` arm in `src/parser/stmt/modifier.rs` lowers a
closure-valued operand to `Expr::CallOn { target: <closure>, args: [$_] }` — a
call with exactly one argument, the topic. That is arity-blind by construction.

The correct lowering is the one the bare-block case now uses: make the closure
the loop's body and let the closure's signature become the loop's signature, so
`Stmt::For`'s existing multi-param handling consumes N elements per iteration
(`for 1,2,3,4 -> $a, $b { ... }` already works).

## Why it is not a one-liner

The `CallOn` lowering also serves `Expr::WhateverCurry` (`* + 1 for @a`) and the
implicit-`@_` block form (`{ @_ } for 1,2`, which rakudo invokes one element at a
time even though its only parameter is slurpy `*@_`). Moving to a
signature-becomes-loop-signature lowering has to keep both of those working, so
it needs a case analysis over the closure's parameter shape (explicit
positionals vs. Whatever-code vs. synthesized slurpy `@_`) rather than a blanket
rewrite.

## Affected files

* `src/parser/stmt/modifier.rs` — `parse_single_modifier`, the `for` arm.
* `src/parser/primary/container/meta_ops.rs` — `try_inline_modifier` /
  `bare_block_body`, which decides which operands reach that arm as a
  `Stmt::Block` rather than a `Stmt::Expr`.
