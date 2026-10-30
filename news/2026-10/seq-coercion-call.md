# `Seq(...)` is the coercion call of Seq

`Seq(1, 2, 'x')` died with "Unknown function: Seq" (Data::Dump::Tree's `t/43_sequences.rakutest`
dies on its first call). It now follows the other container coercions: several arguments are the
elements of a new `Seq`, one argument is its `.Seq`, no argument is the coercion type term
`Seq(Any)`, and a lone type object is the parametric `Seq(Int)`.
