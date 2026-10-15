# A parametric role's sigilless parameter is a term

`role R[\u, $u] { method m { "{u}{$u}" } }` composed as `R[1, 2]` printed `22`:
the role's type-parameter bindings were keyed by spelling, so `\u` and `$u`
shared one entry. Role application now records a sigilless type parameter
under its term key, as ADR-12441 already does for routine and block
parameters, and `C.new.m` prints `12` (#13551).
