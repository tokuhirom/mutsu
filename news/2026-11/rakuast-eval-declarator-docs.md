# `EVAL($ast)` keeps the declarator docs of named declarations

`EVAL(Q[#| docs\nsub k {}\n&k.WHY.Str].AST)` answered an empty string: only an anonymous sub carried its
docs through lowering. A parsed unit hands the runtime a table of documented declarations keyed by the
name `.WHY` and `$=pod` look them up by (`&sub`, `Pkg::method`, `Pkg::$!attr`, `&m/multi.0`,
`R/role.1`); a tree has no parser to produce one, so `rakuast::eval_docs` builds the same table from
the hidden `declarator-docs-*` fields of the tree's declarations, with the parser's keys (multi
candidate and role-variant indices count every declaration, documented or not). `EVAL` installs it for
the unit, and the caller's own table is put back afterwards.

Covered: subs (`multi`, `proto`), methods, submethods, classes, roles, modules, packages, grammars,
grammar rules and attributes. Documented parameters and `my` variables are not (the parser records
neither at runtime either). Fixes the first half of #16787.
