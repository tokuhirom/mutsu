# `require ::($name)`, `require "file"` and `rand` in RakuAST

`require ::($p)` is a `Statement::Require` whose `module-name` is a dynamic `Name`, and
`require "x.pm6"` / `require $f` carry the operand as `file` (with `module-name` an undefined
`RakuAST::Name`); both deparse and lower back. `rand` is `Term::Rand` and deparses without
parentheses.
