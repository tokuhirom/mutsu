# Cheaper `~` on plain operands and faster public-accessor reads

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

A 0-arg plain call whose receiver is an instance and whose name is such a
plain accessor (`self.name` inside a method) is now read before the twenty or
so lanes of the general `CallMethod` preamble, each of which declines that
receiver and name (~2,900 instructions per call).

`bench-class.raku` went from 624.7M to 534.3M instructions (-14.5%) and from
72.0 ms to 63.0 ms wall (min of 15, interleaved; rakupp v5.9.0: 48.6 ms). The
rest of the gap is method-call frames and object construction.
