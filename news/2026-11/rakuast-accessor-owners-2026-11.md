# RakuAST accessor owners re-measured on rakudo 2026.11

The table of RakuAST accessor rows and their declaring classes was generated
from a rakudo 2026.09 measurement. Re-running the pipeline on 2026.11 moves six
accessors to the class 2026.11 declares them in: `infix` of `MetaInfix::Cross`
and `Zip` to `MetaInfix::CrossZip`, `Declaration::Import`'s `lexical-name` and
`compile-time-value` to `Declaration::External(::Constant)`, and
`VarDeclaration::Implicit::Constant`'s `name` and `scope` to
`VarDeclaration::Implicit`, so `.^find_method(...).package` agrees with rakudo.
The hand-written rows that shadowed them moved to those owners or gave way to
the generated accessors (#19059).
