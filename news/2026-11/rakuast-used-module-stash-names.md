# `.AST`: stash names under a `use`d module resolve at parse time

`use Test; Test::EXPORT::DEFAULT::` and `use NativeCall; NativeCall::Types::`
now render as `RakuAST::Term::Name` (they were `Call::Name`), and `use Test; Test`
as a `Type::Simple`, as rakudo 2026.11 does. A `use`d module's package, its
`EXPORT`, `EXPORT::DEFAULT` and `EXPORT::ALL` stashes, and the packages its scan
declared resolve; an unknown name under it stays a `Call::Name` (#18252).
