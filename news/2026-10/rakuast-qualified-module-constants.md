# RakuAST: a loaded module's qualified constants are terms

`use M; M::NAME`, where the module declares `our constant NAME`, now renders in
`.AST` as the `Term::Name` rakudo 2026.09 builds instead of a `Call::Name`, and
the `MUTSU_RAKUAST=1` round trip no longer looks up a routine `&NAME`. The
module export scan collects an `our` constant under its package path as well
as its bare name; a `my` constant stays private to the module. Six more `t/`
files pass under `MUTSU_RAKUAST=1`.
