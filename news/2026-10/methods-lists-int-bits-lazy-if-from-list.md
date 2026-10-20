# `.^methods` lists `Int.bits`, `power-up`, `power-down`, `lazy-if` and `Array.from-list`

Rakudo's `Int` declares `bits` (`Inf` on the type object, a concreteness
error on an instance), `power-up` (`self ** $b`, an `X::Numeric::Overflow`
failure for a negative exponent) and `power-down` (`self ** $b` for a negative
exponent as a `Rat`, or a `Num` past 64 denominator bits; `1.0` for a zero
exponent or a base of one; an `X::Numeric::Underflow` failure otherwise).
`Iterable.lazy-if($flag)` is `$flag ?? self.lazy !! self` on `List`, `Array`,
`Map`, `Hash`, `Seq` and `Range`, and `Any.lazy-if` answers the receiver
whatever the flag. `Array.from-list(\list)` builds an `Array` over a reified
list. Each is now a method-table row, so the call answers and the name
appears in `.^methods` (#14134). `from-slurpy*`, `set-shape`, `is-generic`,
`STORE_AT_KEY` and the other internal Rakudo hooks are still unmodelled.
