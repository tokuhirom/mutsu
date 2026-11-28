# Cheaper `~` on plain operands and a memoized accessor-read gate

Two costs dominated the non-call part of `benchmarks/bench-class.raku`
(#19293): infix `~` and the fast public-accessor read (`$.name`).

Infix `~` ran a dozen coercion probes on an `Int` or `Num` operand (Proxy
FETCH, Seq reify, Failure, zero-denominator Rational, Nil, Code, mixins,
lists, Str subclasses), all of which decline such a value, and then four
Blob probes on every operand pair, each resolving the class name to an owned
`String`. A plain `Str`, `Int` or `Num` now passes the coercion unchanged, and
two operands that are neither an instance nor a holder skip the Blob branches.

The fast accessor read asked the registry four questions on every call: is
the accessor wrapped, is the class a CStruct, does the name belong to the
object protocol, and does the generated accessor win the MRO race against user
methods of that name. Every answer is a pure function of the registry, so the
combined answer is now recorded per `(class, method)` in the
registry-generation-keyed probe memo, and a wrap, augment or new subclass made
after the accessor is hot invalidates it.

`bench-class.raku` went from 624.7M to 588.6M instructions (-5.8%); the
accessor read itself went from 44.7M to 21.3M and `~` from 61M to 48M.
Wall time moved from 82.4 ms to 81.2 ms (rakupp v5.9.0: 48.6 ms); the rest of
the gap is method-call dispatch and object construction.
