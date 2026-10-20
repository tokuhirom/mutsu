# Class methods of built-in types: one type-object lookup

`Promise.start`, `Seq.from-loop`, `Thread.start`, `X::AdHoc.from-slurpy`,
`Supply.from-list`, `List.from-iterator` and the other class methods of
built-in types were found through three owner hooks. They are rows flagged
`CLASS_METHOD` now, and the by-name dispatch asks one lookup for a type
object: the type's own name if it declares class methods, else the nearest
such class on its MRO. A user `class P is Promise` reaches `Promise`'s rows,
an unrelated `Foo.start` reaches none (the old catch-all handed every package
to `Promise`), and a user method of the subclass wins.
