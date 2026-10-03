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
