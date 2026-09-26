# Bench timing results work under mutsu

The `Bench` 0.2.1 distribution now passes its test suite under mutsu. Two
Rakudo compatibility gaps were involved: an ordinary object inherits
`Any.AT-POS` as a one-element positional value, and ordering operators accept a
user-defined `Real` method even when the class does not compose the `Real`
role. Both behaviors now follow the Rakudo dispatch paths without widening
arithmetic or equality coercion.
