# `.pairs` / `.antipairs` / `.rotor` on `Seq.new($iterator)` stay lazy

Calling one of the lazy stream stages on a `Seq` built from a user iterator
drained the iterator first, so an endless iterator hung and a finite one had
its `pull-one` run at call time. The call now hands the unpulled iterator to a
fresh lazy Seq (as `.map` / `.grep` already did) and the stage pulls on demand,
like rakudo (#16132).
