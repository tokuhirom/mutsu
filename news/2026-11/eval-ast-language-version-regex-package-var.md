# EVAL'd units compile under their own language revision; package-variable regex interpolation lowers

Two gaps that kept `t/` files from passing as `EVAL(slurp($file).AST)` (#16356):

- The compile of an EVAL'd unit now sees the unit's own `use v6.x`; the parse
  that read it had already restored the caller's revision, so a `use v6.c`
  unit compiled a stored `rx` with the 6.d topic rule.
- A regex interpolation of a package variable (`$P::v`, `$OUR::x`) lowers from
  a `RakuAST::Regex::Interpolation` whose `var` is a `Var::Package`.
