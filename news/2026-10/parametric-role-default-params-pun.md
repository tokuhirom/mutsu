# A method call on a parametric role binds its default parameters

A method called directly on a parametric role's type object now puns the role
to its default parameterization when every type parameter has a default.
`role E[::R = Any] { method r(R $v) { $v } }; E.r(21)` used to die with
`expected R but got Int (21)`, because `R` was never bound. `$x` in
`role E[$x = 5]` read as `Nil` for the same reason. `.new` already
materialized the default parameterization (`dispatch_new`). The VM's
method-call fast path for role type objects now does the same before it runs
the role bodies, and dispatches on the materialized `E[Any]`.

Found via the `Monad` distribution, whose `Monad::Either[::L = Any, ::R = Any]`
is used as `Monad::Either.right(21)`. All seven of its test files now pass
under mutsu.
