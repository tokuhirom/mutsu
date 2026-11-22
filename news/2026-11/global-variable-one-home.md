# `$*x` falls back to GLOBAL before PROCESS; a GLOBAL variable has one home

A `$*x` that no frame binds now asks `GLOBAL::<$x>` before `PROCESS::<$x>`, as
rakudo's dynamic lookup does, so a `GLOBAL::<$x> = 5` shadows a same-named
`PROCESS::<$x>`.

The `GLOBAL::` stash read from inside a routine also finds root variables now:
`GLOBAL::<$GV>` and `GLOBAL::.keys` see a `GLOBAL::<$GV> = ...` store or a
mainline `our $GV`, which used to be `(Any)` and missing. The `our` store is
the variable's single home: the env copy of the frame that first wrote it no
longer shadows a later write or a `DYNAMIC::<$*x> := ...` bind made by a
routine (#18835, roast `S02-names/pseudo-6e.t` test 202).
