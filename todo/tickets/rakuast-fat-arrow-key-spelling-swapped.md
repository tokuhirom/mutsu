# RakuAST renders the two fat-arrow key spellings the wrong way round

Measured against rakudo 2026.07, mutsu has `a => 1` and `"a" => 1` **swapped**:

| source | rakudo | mutsu |
|---|---|---|
| `a => 1` (bareword key) | `FatArrow(key => "a", value => ...)` | `ApplyInfix(left => QuotedString, infix => "=>", ...)` |
| `"a" => 1` (quoted key) | `ApplyInfix(left => QuotedString, infix => "=>", ...)` | `FatArrow(key => "a", value => ...)` |
| `$k => 1` (computed key) | `ApplyInfix(left => Var::Lexical, infix => "=>", ...)` | boundary (`non-literal pair key`) |

Both wrong cases render *something*, so this is silent wrongness rather than a
coverage boundary — and `a => 1` is one of the most common constructs in Raku.

## Root cause

`src/rakuast/convert.rs` keys the choice off `Expr::PositionalPair`: that variant
renders `FatArrow` and a plain `Expr::Binary { op: FatArrow }` falls through to
the generic infix arm. But `PositionalPair` does not mean "quoted key" — its own
doc says it marks *a pair expression that was parenthesized*. It happens to also
appear for `my $p = "a" => 1`, which is what makes the current mapping look
right in that one case.

Measured internal ASTs:

```
a => 1            Binary { left: Literal(Str("a")), op: FatArrow, ... }
"a" => 1          PositionalPair(Binary { left: Literal(Str("a")), ... })
(a => 1)          PositionalPair(...)     <- bareword key, still PositionalPair
("a" => 1)        PositionalPair(...)
```

So the bareword and quoted spellings both arrive as `Literal(Str("a"))` on the
left, and `PositionalPair` cannot separate them: `(a => 1)` is a bareword key
that would render as an `ApplyInfix` under any rule keyed on that variant, where
rakudo renders a `FatArrow` inside a `Circumfix::Parentheses`.

## What it needs

The parser has to record which spelling produced the key — the same shape of fix
as the `is_sub` flag on `Expr::AnonSubParams` (news/2026-09/
rakuast-anonymous-sub-signature.md) and for the same reason: a distinction raku
models with different nodes that mutsu erases before conversion. A flag on the
`FatArrow` binary (or a dedicated pair node) would do it; `Expr::Binary` is
generic, so it probably wants its own node rather than a field every binary pays
for.

Until then the honest interim state would be to refuse both spellings rather
than render them swapped — that is a judgement call for whoever picks this up,
since refusing removes coverage that some existing tests may assert.

## Minimal repro

```
mutsu -e 'say Q{my $p = a => 1}.AST'      # renders ApplyInfix; rakudo: FatArrow
mutsu -e 'say Q{my $p = "a" => 1}.AST'    # renders FatArrow;   rakudo: ApplyInfix
```
