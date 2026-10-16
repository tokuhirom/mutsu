# Any's coercions, slice and take are late rows of the method table

`Any`'s coercions to `Seq`, `List`, `Set`, `SetHash`, `Bag`, `BagHash`, `Mix`, `MixHash`, `Map`, `Hash` and `hash`,
`Any.slice`, `Mu.take` and `Bag.new-from-pairs` lost their name-matching arms in the interpreter's method cascade. They
are now *late rows* (ADR-11276 §9.136): interpreter rows that only the method-table MRO walk reaches. A plain list keeps
its fast native coercion, and an instance, a type object or a user iterator reaches the row. A type object calling a
method whose candidates all want a defined invocant now dies with `X::Multi::NoMatch`, as in Rakudo:
`Int.slice(0, 2)` and `Int.rotor(2)` used to return a value.
