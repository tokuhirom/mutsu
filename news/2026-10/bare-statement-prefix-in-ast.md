# `.AST` of a statement prefix over a bare statement keeps the statement

`gather say 1`, `try say 1`, `start say 1`, `once say 1`, `BEGIN say 1` and `do say 1` now render in
`Str.AST` as rakudo's `StatementPrefix::<Kind>(Statement::Expression(...))` instead of the
one-statement block the braced form makes. `Spelling::BareStatement` on `Expr::Spelled` (ADR-12199,
slice S3) carries the marker, built only by parses that keep spellings. `do STATEMENT` converts for
any statement (it was refused before), `StatementPrefix::{Do,Try,Gather}.new` can be hand-built and
`EVAL`ed, and `start` over a statement round-trips. The corpus rises to 95.1% identical; checked by
`t/rakuast/rakuast-bare-prefix.t`, which also passes under raku.
