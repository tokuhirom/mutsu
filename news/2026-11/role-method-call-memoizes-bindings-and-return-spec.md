# A role method call stops re-deriving its composition bindings and return spec

Every call to a method composed from a parametric role (`role Box[::T]`)
cloned the consuming class's whole `class_role_param_bindings` map, `String`
keys included, to install `T` into the method's env, and a method with a
`--> Spec` return type re-resolved `Spec` through its owner's package chain
on every call: the existing return-type fast lane is off in any program that
ever bound a type capture, which every parametric role does.

Both are now remembered: the bindings per `(owner, receiver)` and the
`class_role_param_bindings` table state they were read under, the resolved
return spec per `(owner, spec)` and the registry type stamp plus the package
type-alias table. The env-dependent step, resolving a capture this call
bound (`--> T`), still runs per call and bypasses the memo when it applies.

On Monad's type hook (`$maybe.maybe-type`, a `--> Mu` role method) a call
went from ~23.5K to ~18.5K instructions, and one iteration of the
`t/14_concurrent_types.t` chain loop from 11.07M to 10.52M (#17331).
