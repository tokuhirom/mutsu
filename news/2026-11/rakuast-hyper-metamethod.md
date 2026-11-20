# `.AST`: `@a».^name` keeps its hyper

A metamethod call after `».` / `>>.` is not distributed, so the parser handed it
back as the plain call and the written hyper was lost. It is now a
`Spelling::HyperMetaMethod` marker: `.AST` renders `MetaPostfix::Hyper(Call::MetaMethod)`
as rakudo 2026.11 does, and `EVAL` of that tree lowers it to the same undistributed
call (#18273).
