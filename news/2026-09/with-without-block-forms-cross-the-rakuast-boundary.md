# The `with` / `without` block forms cross the RakuAST boundary

`with X { … }`, `without X { … }` and the `orwith` / `else` clauses that
continue them executed correctly but could not be rendered as RakuAST at all:
`.AST` reported the parser's desugar back as an unsupported construct
(`DoStmt(VarDecl { name: "__with_tmp_0", … })`). They now render as
`RakuAST::Statement::With` / `::Without` / `::Orwith`, byte-identical to rakudo
2026.07, and a hand-built node of any of those lowers back to the same
executable shape.

This is the block half of the slice whose statement-modifier half shipped as
[#8036](https://github.com/tokuhirom/mutsu/issues/8036); it closes
[#8123](https://github.com/tokuhirom/mutsu/issues/8123).

## The desugar, and why the shape could not simply be recognised

mutsu has no runtime representation of `with`. The parser rewrites the block
form into a conditional over a once-evaluated temp:

```
with X { BODY }   ->   if (my $__with_tmp_N = X).defined { given <topic> { BODY } }
```

with the condition negated for `without`, an `orwith` clause becoming a nested
`if EXPR.defined { given EXPR { … } }` in the else branch, and a trailing `else`
wrapped in a `given` of its own so it topicalizes on the last tested value.

Nothing of the source keyword survives that: a hand-written
`if (my $t = 1).defined { given 1 { … } }` produces the same statement, and the
synthetic temp's *name* is not a discriminator either — a program may declare
`__with_tmp_0` itself, and a converter that keyed off it would be guessing. So,
as for `unless` (`is_unless`) and `until` (`is_until`) before it, the
distinction is kept rather than reconstructed:

- `Stmt::If` grows `with_kind: Option<WithBlockKind>` (`With` / `Without` /
  `Orwith`), set by `parser::stmt::control::with_stmt` and by the `orwith` arm
  of `parse_elsif_chain`;
- `Stmt::Given`'s existing `with_kind` grows a `BlockTopic` variant marking the
  scaffold `given` a block body runs under — which raku does not model as a
  `given` at all, but as the `implicit-topic => True` / `required-topic => 1`
  flags on the `Block` itself.

Execution ignores both markers; only the RakuAST converter reads them.

Only the *parameterless* spelling is marked. A pointy body (`with X -> $a { … }`)
binds its parameter inside the same scaffold `given`, which raku spells as a
`PointyBlock` rather than an implicit-topic `Block`, so it is deliberately left
unmarked and `.AST` reports the boundary instead of silently dropping the
parameter.

## Both directions

- `src/with_desugar.rs` (new) holds the pieces the parser and the RakuAST
  lowerer both need — the temp-name counter, the `(my $tmp = X).defined`
  condition, the topic-routing rule (an lvalue or literal condition topicalizes
  on the source, anything else on the temp), and the two conditional builders —
  so the desugar is spelled once instead of twice.
- `convert.rs` renders the node, recovering the written condition from the
  `.defined` test and the block body from the scaffold `given`. The `elsifs` /
  `else` walk is now shared with `Statement::If`, which gained `orwith` clauses
  for free: `if 1 { } orwith 2 { } else { }` is legal Raku and renders correctly
  too. An `else` topicalizes exactly when the clause it continues does, matching
  rakudo (after `orwith`: yes; after `elsif`: no).
- `lower.rs` accepts hand-built `Statement::With` / `::Without` / `::Orwith`
  nodes and rebuilds the parser's shape, so the round trip is stable and
  `EVAL(Q[…].AST)` runs on the existing compiler and VM path.

A `with` block written *inside* an `else` stays a statement rather than being
absorbed as a continuation clause, which the shared chain walk checks for
explicitly.

## Verification

`.AST.gist` is byte-identical to rakudo 2026.07 for `with 1 { say 2 }`,
`without Nil { say 2 }`, `with … else`, `with … orwith … else`,
`with … elsif … else`, `with … orwith … orwith …`, `if … orwith … else`, and a
`with` nested in an `else`; `EVAL(Q[…].AST)` agrees with rakudo on all of them,
including the single evaluation of the condition and the write-back through an
lvalue topic.

Pinned by `t/rakuast/rakuast-with-without-block.t` (25 tests: node classes, the
`then` / `body` naming asymmetry, the topic flags, the `elsifs` chain, which
`else` topicalizes, `EVAL` of the round-tripped AST, and the source-level
semantics). It passes under rakudo unchanged.

One assertion there is spelled against `.gist` rather than `.elsifs`, because an
absent optional field still throws on mutsu instead of returning an empty list —
that gap is [#8124](https://github.com/tokuhirom/mutsu/issues/8124), unchanged
by this work.
