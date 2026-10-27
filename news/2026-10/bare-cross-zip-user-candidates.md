# Bare `X` / `Z` take the user's `infix:<X>` / `infix:<Z>` candidates

The bare cross and zip metaoperators are the routines `infix:<X>` and
`infix:<Z>`: a `multi` candidate that a `use` imports (Red's
`multi infix:<X>(+sources where { ... })`) joins the core candidate and wins the
calls it accepts. The RakuAST lowering cannot see an import, so a statement such
as `my $d = (A.^all X B.^all).grep({ ... })` lowered to the core cross product
and the block received the two `ResultSeq`s instead of the model aliases
(#15054, Red `t/91-cross-product`). `MetaOp` / `MetaOpNary` with an empty
operator now ask the user candidates first (a block-scoped `use` included) and
fall back to the built-in operator when none accepts the operands.

A declared bare `X` or `Z` also stops claiming the front of a longer
metaoperator token: `X+` and `Z~` stay the metaoperators, as in rakudo.

A parser-known user `infix:<X>` / `infix:<Z>` is list-associative by default, so
`$a X $b X $c` is one call with three operands, not `X(X($a, $b), $c)` (#15056).
