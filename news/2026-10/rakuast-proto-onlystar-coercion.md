# RakuAST: proto routines, `{*}` and coercion constraints

A re-survey after the attribute-trait slice found 101 `t/` files that stopped
at `.AST` on a `proto` declaration, and 103 at a coercion type, most of them
with an explicit constraint such as `Int(Cool)`. Measured on rakudo 2026.09:

- `proto sub f(|) {*}` and `proto method m(|) {*}` are the ordinary `Sub` /
  `Method` node led by `multiness => "proto"`.
- A body that is only `{*}` is `body => OnlyStar` itself. `OnlyStar` is a
  `Blockoid`. A `{*}` among other statements, as in
  `proto g($x) { "<" ~ {*} ~ ">" }`, is an `OnlyStar` expression.
- `Int(Cool)` is a `Type::Coercion` whose `constraint` is the type the value
  is coerced from. `Int()` has none.

The parser keeps the two `{*}` forms apart already: the whole-body form as the
`Whatever` term, the inner one as the onlystar dispatch call. The converter now
recognises the latter through the new `Expr::is_onlystar_dispatch`, next to
the constructor that builds it. A proto's `is export` goes through the routine
`IsTraits`. `ProtoDecl` records an untagged export as an empty tag list where
`SubDecl` uses `DEFAULT`, so the bridge maps between the two.

Protos with custom traits, `our` scope or a return type still decline.

The round-trip ratchet grew by 75 files, to 3238.
