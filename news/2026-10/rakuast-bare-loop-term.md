# RakuAST: parenthesised `loop` and `repeat` terms

`(repeat { ... } while COND)` and `(repeat { ... } until COND)` now parse as
terms (they used to die with "Malformed initializer"), and `(loop { ... }).m`
keeps its parentheses in `.AST` like a parenthesised `while` already did
(`Stmt::Loop` gained the compiler-ignored `is_bare_term` flag). rakudo's
`.not` on a parenthesised `until` condition is deliberately not copied: it
double-negates and makes rakudo itself skip the loop.
