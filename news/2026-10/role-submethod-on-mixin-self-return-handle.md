# A role's BUILD/TWEAK on a `but`/parameterized `does` mixin sees `self` and may `return`

A role submethod run on a mixin wrapper (`K.new but R`, `$x does R[Int]`,
`$*ARGFILES does R[:reset]`) read `self` as `Nil` (the compiled body seeds it
from env), and a `return` in it escaped as "Attempt to return outside of
immediately-enclosing Routine", aborting the composition. `self` is now bound,
and a `return` just ends the submethod, as it does for a role composed into a
class. A handle with a role mixed in (`IO::Handle+{R}`) is also recognised as
the open handle, so `lines()` keeps reading through `$*ARGFILES does R`.
Found via LN (`IO::CatHandle::AutoLines`): `use LN` now loads and mixes the
role into `$*ARGFILES`; the line counter still needs `$*ARGFILES` to be a real
`IO::ArgFiles` (#NEW).
