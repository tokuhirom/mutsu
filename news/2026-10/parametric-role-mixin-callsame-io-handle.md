# `callsame` in a parameterized role mixed into an open handle reaches the native method

`$fh does R[:reset]` (a parameterized role) keeps the handle as the inner value of a mixin
wrapper, and the role's `callsame` to the native `IO::Handle.get` returned `Nil` instead of
reading a line (the non-parameterized role, reblessed to `IO::Handle+{R}`, already worked).
The base behind the callsame is now the native method on the wrapped plain handle. Found via
LN (`IO::CatHandle::AutoLines`): `$*ARGFILES.get` under `-MLN` now returns the line and counts it.
