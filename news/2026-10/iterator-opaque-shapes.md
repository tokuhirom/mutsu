# Built-in iterators are opaque shapes decoded from their attributes

A built-in iterator is an instance of one class, `Iterator`, that is also a
`PredictiveIterator` only when it knows its count. `DispatchShape::Iterator`
and `PredictiveIterator` are opaque shapes now, chosen from the instance's
attributes (`value/dispatch_shape/iterator_kind.rs`), and the generic opaque
owner lookup answers the protocol rows. `iterator_owners_of` and two of its
four call sites are gone; the `.^can` and `CallMethodMut` fast-path sites ask
the shared opaque lookup.
