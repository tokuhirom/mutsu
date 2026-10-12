# A type object rolls with a count, and `Mu` loses `Any`'s list methods

`Int.roll(2)` and `Any.roll(3)` raised `X::Method::NotFound`: the shared
`roll($count)` implementation declined every type object so an enum type
object could reach the interpreter's enum sampler. The enum case is now
routed by its own check (the VM sends only an enum type object to the
interpreter), so any other type object rolls itself through the `Any.roll`
row, as in Rakudo (#12958). `Mu.tail`, `Mu.unique` and `Mu.minmax` now raise
`X::Method::NotFound` instead of answering, since `Mu` is not an `Any`.
