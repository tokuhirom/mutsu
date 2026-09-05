# RakuAST renders the two fat-arrow key spellings the right way round

Measured against rakudo 2026.07, mutsu had `a => 1` and `"a" => 1` **swapped**,
and refused the computed spelling outright:

| source | rakudo | mutsu (before) |
|---|---|---|
| `a => 1` (bareword key) | `FatArrow(key => "a", value => …)` | `ApplyInfix(left => QuotedString, …)` |
| `"a" => 1` (quoted key) | `ApplyInfix(left => QuotedString, …)` | `FatArrow(key => "a", …)` |
| `$k => 1` (computed key) | `ApplyInfix(left => Var::Lexical, …)` | boundary (`non-literal pair key`) |

Both wrong cases rendered *something*, so this was silent wrongness rather than
a coverage boundary — in one of the most common constructs in Raku.

## `PositionalPair` was carrying two meanings

The converter keyed the choice off `Expr::PositionalPair`: that variant rendered
`FatArrow` and a plain `Expr::Binary { op: FatArrow }` fell through to the
generic infix arm. But `PositionalPair` does not mean "quoted key" — it means
*this pair is not a named argument*, which is true of a quoted key, of a
computed key, **and** of a parenthesized bareword one. `(a => 1)` is a bareword
key that the paren parser wraps in the same marker, so no rule keyed on that
variant can separate the spellings.

The distinction does exist at parse time — `fat_arrow_result` keys on
`is_bareword` and only wraps the non-bareword spellings — it was just erased by
the paren parser reusing the marker. So rather than adding a flag to
`Expr::Binary` (which every binary would pay for) or marking the key literal
(whose readers, ~16 sites matching `Expr::Literal` on a pair key, would fail
*silently* if one were missed), the paren parser now records the
parenthesization itself: `(a => 1)` becomes `PositionalPair(Grouped(pair))`.
The `PositionalPair` marker every call-argument path keys on stays on the
outside, and `Grouped` — already documented as transparent to the compiler and
already in most walkers' see-through lists — carries the new bit.

With the two meanings separated, the rendering keys off the pair's own shape:

- a bare `Binary { op: FatArrow }` with a string-literal key is the **bareword**
  spelling (it is a named argument) → `FatArrow`;
- a `PositionalPair` over one is a quoted or computed key → `ApplyInfix`,
  rendered directly rather than by recursing, so it cannot fall into the
  `FatArrow` arm;
- a `PositionalPair(Grouped(…))` is parenthesized → the existing `Grouped` arm's
  `Circumfix::Parentheses(SemiList(Statement::Expression(…)))`, with whichever
  of the two nodes belongs inside.

That last one is a bonus: `(a => 1)` now renders exactly as rakudo does,
parentheses included, where before it rendered a bare `FatArrow`.

The write direction needed the same correction, and had the same swap: a
`FatArrow` node now lowers to a bare `Binary{FatArrow}` (a **named** argument)
and an `ApplyInfix` over `=>` lowers to a `PositionalPair` (a **positional**
Pair), so `EVAL(Q{sub f(:$a) { $a }; f(a => 7)}.AST)` binds `$a` and
`f("a" => 7)` passes a Pair.

## Scope

Colonpairs (`:foo`, `:foo(1)`) share the bare-`Binary{FatArrow}` shape and so
render as a `FatArrow` too, where raku has a distinct `ColonPair::*` family.
That divergence is unchanged in kind: before this, they rendered as an
`ApplyInfix` claiming a quoted-string left operand they never had.

`("a" => 1)` still loses its parentheses (a quoted-key pair arrives already
wrapped, so the paren parser's marker does not reach it) — that is
`todo/tickets/rakuast-parentheses-not-preserved.md`, unchanged.

Pinned by `t/rakuast-fat-arrow-key.t`: 18 assertions across all three key
spellings, argument position, and both directions, passing identically under
rakudo 2026.07 and mutsu.
