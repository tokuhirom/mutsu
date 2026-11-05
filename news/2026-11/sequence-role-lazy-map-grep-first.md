# `map` / `grep` / `first(matcher)` of a `does Sequence` class are lazy

A class that `does Sequence` and supplies its own `iterator` drained that
iterator into a List for every method that is not a prefix consumer, so
`Sq.new.map(* * 2).head(3)` or `Sq.new.first(* > 5)` over an endless iterator
hung. `map` and `grep` now run on a lazy `Seq.new($iterator)` over the class's
iterator (a following `head(n)` pulls only what it needs), and `first(matcher)`
pulls one element at a time and stops at the first match. The same
`first(matcher)` early exit now also applies to a plain `Seq.new($iterator)`
(`first_over_map_grep_seq`), with the matcher run once per element pulled.
