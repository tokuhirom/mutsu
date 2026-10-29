# A parametric role's lexical `sub` is found by a runtime mixin again

`@a does R[:c(1)]` mixes a role with a value parameter into a variable at run
time. Since #15100 the role body's `sub`s are re-declared per composing class
and the parameter-less compunit copy is dropped, but a mixin's role methods
run in the role's own package, so the re-declaration was never found and the
call died with `Unknown function`. The copy is kept for a mixin type. Found
as a regression of Grid (#15423).
