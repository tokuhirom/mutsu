# RakuAST drops parentheses: no `Circumfix::Parentheses` is ever rendered

Split out of `todo/tickets/rakuast-unless-and-parens.md` on 2026-09-05, whose
other half (`unless` rendering as a negated `if`) was fixed separately — see
`news/2026-09/rakuast-unless-and-statement-modifiers.md`. The two were filed
together as "the parser erased a distinction raku keeps", but only the `unless`
half was cheap: that one needed a flag on one `Stmt`, this one needs the parser
to stop discarding a node it discards nearly everywhere.

## The divergence

rakudo keeps parentheses as a `Circumfix::Parentheses` wrapping a `SemiList`:

```
$ raku -e 'say Q{my $x = (1, 2)}.AST'
    ... RakuAST::Circumfix::Parentheses.new(
          RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::ApplyListInfix.new(
                infix    => RakuAST::Infix.new(","),
                operands => (IntLiteral(1), IntLiteral(2))))))
```

`(1)` gets the same wrapper around a bare `IntLiteral`. mutsu renders the
operand with no wrapper at all.

## Why it is not the initializer's fault

The original ticket guessed the initializer path unwraps a `Grouped`. It does
not — the parser never builds one here. `--dump-ast` on `my $x = (1, 2)` gives a
bare `ArrayLiteral([Literal(1), Literal(2)])`, byte-identical to what
`my $x = 1, 2` produces for its first item, and `say (1, 2)` loses the parens
the same way. `convert.rs` already renders `Circumfix::Parentheses` correctly
for `Expr::Grouped`; it simply never receives one.

`Expr::Grouped` exists and is documented as "marks a parenthesized expression so
the compiler can distinguish `(1|2)|3` from `1|2|3`", but
`parser/primary/container/paren.rs` wraps only a narrow allowlist —
junction/`BareWord`/`Feed`/scalar-`Var`/assignment/`X`/`Z` meta-op results, plus
the whatever-currying freeze — because each entry buys a specific downstream
behaviour. Everything else is returned unwrapped.

## Why it is large

Making `paren_expr` wrap unconditionally is the obvious fix and is a wide
semantic change, not a rendering one: `Expr::Grouped` is *not* uniformly
transparent downstream. Consumers pattern-match the unwrapped shape all over
(`Expr::ArrayLiteral`, `Expr::Binary`, `Expr::MetaOp`, the argument-list lift,
the for-loop rw-source detection, the sink-warning analysis), and several of
them unwrap `Grouped` only where someone needed them to. Turning it on
everywhere would surface every place that does not, as roast failures spread
across unrelated synopses.

The tractable shapes of the work, in increasing order of blast radius:

1. Wrap unconditionally, then fix every consumer that breaks. Honest, and the
   safety net (`make roast`) is exactly the right one — but it is a campaign,
   not a slice.
2. Carry the parenthesization as a flag rather than a wrapper node, so no
   pattern-match sees a new variant. Cheaper to land, but it is the same
   "a field every expression pays for" objection raised in
   `todo/tickets/rakuast-fat-arrow-key-spelling-swapped.md`.
3. Wrap only in the positions RakuAST conversion actually reaches and the
   compiler provably ignores. Narrow, but it leaves the AST inconsistent about
   what a `Grouped` means, which is how this ticket's sibling
   (`PositionalPair` meaning two different things) came about.

Deciding between them wants a measurement pass over the `Grouped` consumers
first, which is why this is a ticket rather than a slice.

## Already covered

One shape is done and must not regress: a **lone parenthesized bareword pair**
(`(a => 1)`) renders `Circumfix::Parentheses(SemiList(Statement::Expression(
FatArrow)))`, matching rakudo. It fell out of
`news/2026-09/rakuast-fat-arrow-key-spellings-unswapped.md`, which needed the
paren parser to record parenthesization anyway in order to tell a parenthesized
bareword key from a quoted one — so `paren.rs` now emits
`PositionalPair(Grouped(pair))` for exactly that shape. `("a" => 1)` is NOT
covered: a quoted-key pair arrives already wrapped in `PositionalPair`, so the
`Grouped` marker never reaches it.

That is approach (3) above applied to a single shape, and it is a worked example
of its cost: the AST now says "parenthesized" in one place and nowhere else,
which is precisely the inconsistency this ticket has to resolve.

## Repro

```
mutsu -e 'say Q{my $x = (1, 2)}.AST'   # no Circumfix::Parentheses
mutsu -e 'say Q{my $x = (1)}.AST'      # likewise
mutsu -e 'say Q{say (1, 2)}.AST'       # not initializer-specific
mutsu --dump-ast -e 'my $x = (1, 2)'   # the parser produced no Grouped
```

No fixtures.
