# EVAL'd units: exit phasers, their own language revision, package-variable regex interpolation

Three gaps that kept `t/` files from passing as `EVAL(slurp($file).AST)` (#16356):

- A unit-level `LEAVE`, `KEEP` or `UNDO` of an EVAL'd unit was dropped by the
  compiler; they now run when the unit ends, picked by its outcome as the main
  program does (so a variable trait that adds a unit-level `LEAVE` phaser works).
- The compile of an EVAL'd unit now sees the unit's own `use v6.x`; the parse
  that read it had already restored the caller's revision, so a `use v6.c`
  unit compiled a stored `rx` with the 6.d topic rule.
- A regex interpolation of a package variable (`$P::v`, `$OUR::x`) lowers from
  a `RakuAST::Regex::Interpolation` whose `var` is a `Var::Package`.
