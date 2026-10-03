# A default-parameterized role pun is named after the role

`role E[::R = Any] { }; E.new.^name` said `E[Any]`, and a method of a role
like that saw `self.WHAT.raku` as `F[Any]`. Rakudo names the pun after the
role: `E`, `F` (#11281).

Punning such a role binds its parameters to their defaults and composes a class
for that parameterization. That class used to be registered under the spelled
parameterization `E[Any]`, the same name as an explicit `E[Any].new` pun. The
default pun now has a storage name of its own, `E\0default[Any]`, which displays
as `E`. `.new` and a method call on the bare role both go through it. An
explicit `E[Int].new` keeps the name `E[Int]`. Two differences remain and are
tracked in #11535: `.^roles` of the pun still shows `E[Any]`, and the pun still
smartmatches against an explicit `E[Any]`.
