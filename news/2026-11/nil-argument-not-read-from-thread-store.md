# A `Nil` argument is not replaced by a stale cross-thread value

Once a thread had been spawned, a parameter bound to `Nil` (`sub f(Mu $value)` called
as `f(Nil)`) read back, through the name-keyed cross-thread variable store, the value
an earlier call's identically named `$value` had written. A parameter slot is bound by
its own call, so its `Nil` is now the argument. Found via the Red distribution
(`t/038-equal.rakutest`, where `first(* > 100)` on a Seq "returned" the previous
case's value).
