# `.polymod` on a mixed-in number divides the number it wraps

`($n but Role).polymod(2 xx *)` returned an empty list. Its infinite-divisor
path read the mixin as 0 and stopped at once. The receiver now divides the
value it wraps.

Bitcoin's test suite builds its private key as `$int but Bitcoin::PrivateKey`.
secp256k1's scalar multiplication sums `$n.polymod(2 xx *) Z* @doublings`, so
`G * $key` came back as an `Int` instead of an EC point, and `t/basics.t` died
at its first assertion.

In the same test, `$key.address` then failed to dispatch: `UInt` rejected a
mixed-in Int even when the number it wraps is non-negative. Both a smartmatch
and a `UInt` parameter now accept it.

Finally, `P2PKH::address self` inside `role Bitcoin::PrivateKey` names the
module's own `our package P2PKH`. A qualified call from a method (or a closure
or lexical sub inside one) of a class or role declared in a module now finds
`Q::f` under the enclosing package too, as Rakudo finds it through the lexical
scope. It used to work only from the module's own subs.

The `checkedB58Str` subset then calls `Base58::decode ~$/` inside its code
assertion. A package-qualified routine called as a listop now takes an
argument that opens with a glued prefix operator (`M::f ~$x`, `M::f -1`), so
this no longer parses as `M::f() ~ $x`. A spaced infix (`M::c - 1`) is still
an infix. The bare-name form inside a regex code block is #11616.
