# `.all` / `.any` / `.one` / `.none` on a Positional object use its elements

`Any.all` is the junction of `self.list`. For an instance of a class that is
`Positional` (a `CArray`, a user collection), mutsu wrapped the object itself,
giving `all(CArray.new)`, so `$carray.all ~~ Numeric:D` was False.
`LLM::RetrievalAugmentedGeneration`'s `nearest(... where $vec ~~ Positional:D &&
$vec.all ~~ Numeric:D)` therefore rejected every `CArray[num64]` vector with
"Cannot resolve caller nearest". The four junction methods now take the object's
`.list` when it is `Positional`. A class that only does `Iterable` is a separate
gap (rakudo expands it through its iterator).
