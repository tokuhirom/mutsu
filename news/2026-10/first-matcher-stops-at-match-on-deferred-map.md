# `.first(matcher)` on a deferred `.map` Seq stops at the match

`(1, 2).map({ slip(1..*) }).first(* > 4)` now answers `5`, and
`(1..10).map({ $calls++; $_ }).first(* > 2)` runs the callback three times, as Rakudo does.
A not-yet-run `.map`/`.grep` Seq used to be read whole before `first` tested its elements,
which died on a callback that slips an infinite list and ran every callback otherwise.
`Interpreter::first_over_map_grep_seq` now pulls such a Seq one element at a time through
the prefix pull and consumes it.
