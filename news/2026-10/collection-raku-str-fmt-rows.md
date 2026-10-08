# `raku`, `Str` and `fmt` of the collections are rows

`raku` of `Array`/`List`/`Hash`/`Pair`/`Seq`/`Capture`, `Str` of `List`/`Hash`/
`Pair`/`Seq`/`Capture`/`Range`, `Capture.gist`, `Seq.Stringy` and `fmt` of the
collections and the six quant hashes are rows of the method table now
(ADR-11276 §9.38). `fmt` was three copies, one per arity cascade, plus a fourth
in the interpreter; the three are one function behind the rows, and the
interpreter's copy is the path a declined call (a `Format` object, an item with
its own `Str`) takes. The `raku` row declines to the interpreter exactly where
the cascade's gates did, so a typed container and an element with its own `raku`
render as before.
