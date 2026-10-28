# A class in an `EVAL` whose method trait dies is no longer left installed

When an `EVAL`'d unit died while compiling a class declaration (a method trait whose
`trait_mod:<is>` throws), the class stayed registered and the next `EVAL` declaring the same
name died with `X::Redeclaration`; rakudo discards the failed unit. The VM now records the
classes a dying declaration registered inside an `EVAL`, and `isolated_eval_unit` leaves them out
of the registry it merges back (including their method rows). A class that compiled and then died
at run time still stays declared, as in rakudo. `AccessorFacade`'s `t/020-fail.t` shape now
passes. Covered by `t/oo/trait/eval-class-trait-die-not-installed.t`. A `BEGIN` that dies inside
the class body is a separate path and is not covered yet.
