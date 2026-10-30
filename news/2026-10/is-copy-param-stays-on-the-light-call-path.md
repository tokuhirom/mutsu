# A plain `$x is copy` parameter stays on the light call path

A positional `$x is copy` parameter (with or without a constant default) used to push the whole
routine off the positional-light call path, because that path admitted only parameters without
traits (or with exactly `is rw`). The call then went through the general binder: a full Env clone,
a call frame and per-parameter bookkeeping. `Test.rakumod`'s `proclaim` takes `$desc is copy`, so
every `ok`/`is`/`is-deeply` paid it.

The light bind now admits a single `copy` trait on a plain `$` scalar: the argument is bound by
value (the itemize mode already followed `scalar_param_bind`) and the parameter is left writable,
as the general binder does. A constant default (`$t is copy = "z"`) is filled from the same
precomputed constant, which is immutable, so a write to the copy cannot reach the next call.

Measured with callgrind (profiling build, slope between 500 and 1500 iterations, startup
subtracted, both binaries built from the same `main`): `sub f($desc is copy) { $desc }; f("x")`
21,741 -> 6,132 Ir per call, and `sub f($s is copy, $t is copy = "z") { $s ~= $t; $s }; f("a")`
25,052 -> 7,928. A light-bound copy also owns its name's type metadata, as the general binder
makes it: a typed one registers its constraint, an untyped one drops an inherited same-named
caller constraint.

This does not speed up `Test::ok`/`is` yet (190.7k -> 189.9k Ir per `ok`, -0.5%):
`proclaim` also takes a `Bool(Mu)` coercion parameter, which still sends it through the general
binder (+18.8k Ir per call measured separately).

As a side effect the light path matches rakudo where the general binder does not: `$x is copy`
bound to an Array shares the array (`$x.push(9)` is seen by the caller), while the general binder
copies it. That divergence between the two paths is filed separately.
