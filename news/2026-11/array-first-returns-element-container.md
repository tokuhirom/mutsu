# `Array.first` answers the element's own container

`@a.first` and `@a.first(matcher)` on a mutable `Array` now return the matched
element's container, as `.head` already did and as Rakudo does:
`@a.first(* > 1).VAR.^name` is `Scalar` and `my $x := @a.first(...)` binds into
the array. `List`/`Seq` receivers and the `:k`/`:kv`/`:p` forms still answer
values. A `.map` Seq whose block returns an element container is tracked
separately in #17072.
