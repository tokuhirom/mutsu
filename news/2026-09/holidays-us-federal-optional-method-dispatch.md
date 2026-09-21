# Holidays::US::Federal reaches parity

`Holidays::US::Federal` 0.0.5 moved from partial (1/4 baseline files) to green
(4/4 files, 233/233 assertions) in the ecosystem sweep.

Its `Date::Event.etype` method has two typed optional multi-method candidates,
`Str $v?` and `UInt $v?`. When the optional argument was omitted, mutsu treated
them as equally narrow and raised an ambiguous-dispatch error. Method
narrowness now recognizes the builtin `UInt` subset, so the same zero-argument
dispatch selects the `UInt` candidate as Rakudo does.

Pinned by `t/routines/dispatch/multi-optional-typed-dispatch.t` and measured in
the sandbox with Rakudo 2026.07.

This run was locked on [#7884](https://github.com/tokuhirom/mutsu/issues/7884).
