# RakuAST::Name.qualified-with

`RakuAST::Name.qualified-with($target)` now works: the target's parts come
first, and a name that starts with `GLOBAL` answers its own parts without that
part (the qualifier is ignored), as on rakudo 2026.09. A target that is not a
`Name` raises `X::TypeCheck::Binding::Parameter`. The cause of the old "No such
method" was that the `RakuAstNode` dispatch shape listed only some node classes
as row owners; `RakuAST::Name` is now one of them, so any one-argument row on a
`Name` is reachable (#16718).
