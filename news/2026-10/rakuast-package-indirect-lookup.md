# RakuAST keeps the package of `MY::(…)` and `GLOBAL::(…)` lookups

`MY::(q[$x])`, `CALLERS::('$_')` and `GLOBAL::('Int')` look a symbol up
under a written package. The parser joins the package and the key into one
string expression, so `.AST` rendered a `Var::Package` over an empty first
part. That node does not exist in rakudo's tree, and `MUTSU_RAKUAST=1` could
not lower it.

The parser now records the package and the key separately when it keeps
spellings for RakuAST. The converter renders what rakudo 2026.09 renders: a
`Term::Name` over `Name(Part::Simple("MY"), Part::Expression(…))` for a
lexical pseudo-package, and a `Call::Name` over the same name for `GLOBAL`,
`PROCESS`, `EXPORT` or a user package. Lowering builds the string lookup
back. Three more `t/` files pass under `MUTSU_RAKUAST=1`.
