# A straight-line role method on a mixin runs without a method frame

`CArray[num64]`'s `ASSIGN-POS` and `AT-POS` are role methods whose bodies are
a handful of local reads and one `nqp::` op. Once the winner memo has decided
the candidate, they no longer go through the method machinery at all
(`Interpreter::run_straight_line_method`): `self` and the positional
arguments are written straight into the body's locals, the ops run, and the
locals frame is popped. No scoped env, routine frame, attribute snapshot,
parameter binder or `Mixin`-wide attribute commit is built for them.

Callgrind slope on a profiling build (second run, N 500 -> 1500), per element:
a `$c[$_] = 1.5e0` store on `CArray[num64]` 60.0k -> **40.3k Ir (-33%)**;
fill + `.Array` (the `Sort::Fast` shape) 136.4k -> **99.3k Ir (-27%)**.

The lane declines (and the general path runs) for a body that reads anything
but `self` and its parameters, for named, slurpy, defaulted, trait-carrying or
sigilled parameters, for native-integer and `Positional` parameters (they bind
through a coercion), and for `Proxy`, container or pair arguments.

Refs #16215.
