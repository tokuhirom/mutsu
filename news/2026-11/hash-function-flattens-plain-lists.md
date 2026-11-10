# `hash(LIST)` flattens the plain Lists among its elements

`hash(('A' .. *) Z @vals)` died with "Odd number of elements": the `hash` routine expanded each
argument one level only, so a Seq of 2-element Lists reached the hash initializer as Lists.
Rakudo's `hash(+@list)` flattens the plain Lists (a zip, a `List` of `List`s), while an `Array`'s
itemized elements stay whole. `hash` now uses the same plain-list flattening `Hash.new` does
(found by Qwiratry's `halfhuman-format.rakutest`).
