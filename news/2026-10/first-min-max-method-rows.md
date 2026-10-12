# `.first`, `.min` and `.max` lose their name-matching arms

`.first` with no matcher now answers the first element of the invocant's
list for every receiver, through the `Any.first` row (ADR-11276 §9.85):
a `Range`, a `Hash` or a quant hash (its first `Pair`), a `Capture`, a
`Blob` or a list mixin used to answer itself. `.min` and `.max` answer from
the `Any` rows, which read a `Pair` as its one element. Three more arms
leave the pure arity cascades (303 to 300).
