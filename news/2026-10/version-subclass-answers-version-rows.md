# An `is Version` subclass answers `Version`'s rows

`Version.ACCEPTS` had a row, but the row took no `Version` argument (its
arguments had to be plain scalars), so every smartmatch between versions went
through a 1-argument cascade arm that also unwrapped an `is Version` subclass
instance. The row now takes a `Version` argument, or a subclass instance through
the version it carries, and the native-base redirect knows the `Version`
payload, so a subclass instance answers `Version`'s rows like an `is Range` or
`is Pair` one (ADR-11276 slice 5). The arm is gone, and so is the arm that
numified a `Str` argument of `base` and `polymod`: the rows read it themselves.
