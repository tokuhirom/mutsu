# RakuAST: a signature declaration's `is default(...)` is a `Trait::Is`

`my ($a, $b) is default(42)` kept its group default as a hidden
`group-default` expression on its `VarDeclaration::Signature`. `.traits`
died with "No such method", `visit-children` walked a bare `IntLiteral`, and
the declaration's signature and parameters had no origins: the origin pass
read the declaration's text only when it ended with the signature's `)`.

The default is now a `Trait::Is(name => default, argument =>
Circumfix::Parentheses(...))` in the declaration's `traits`, and in each
parameter's `VarDeclaration::Simple`, as in Rakudo 2026.11. `.raku` still
leaves it out, as Rakudo's does. The traits are visited after the
initializer and placed through the declaration trait reader
`decl_origins` already has. `EVAL` of the tree keeps the default (#18597).
The constructor and the trait code live in the new `signature_decl_traits`
module, which takes `signature_decl.rs` from 551 lines to 518.
