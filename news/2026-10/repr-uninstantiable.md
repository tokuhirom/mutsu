# `is repr<Uninstantiable>` is reported and refuses construction

Upstream `NativeCall::Types` declares `our class void is repr<Uninstantiable> { }`.
mutsu ignored that REPR, so `void.REPR` answered `P6opaque` and `void.new`
built an instance. Upstream's `check_routine_sanity` follows a `Pointer`
parameter's `.of` to `void` and rejects anything reporting `P6opaque`, so
every `sub f(Pointer) is native` declaration warned "Not an accepted
NativeCall type" once `use NativeCall` loads the vendored module (#11203).

A class declared `is repr<Uninstantiable>` now reports that REPR, and `.new`
and `nqp::create` on it die with rakudo's "You cannot create an instance of
this type" (part of #11209).
