# A method call with a named argument stops cloning its arguments to key them

The plain-method lane keys every call by its arguments' types. Building that key
cloned each argument (a cloned named-argument `Pair` dropped back into the GC
candidate buffer), derived the type name of well-known scalars only to throw it
away, and the eligibility check for a shared container cloned every `Pair` too.
The key now borrows its arguments, the scalar name is derived only for types
without a well-known symbol, and the named-parameter scan tests the pair tag before
decoding. Refs #15443.
