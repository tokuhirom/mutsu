# `EXISTS-POS` with a negative index on an infinite lazy list

`(1..Inf).map(* + 1).EXISTS-POS(-1)` died with "Cannot coerce an infinite lazy
list to a strict list"; a negative index never exists, so the lazy step's
`EXISTS-POS` pull answers `False` before pulling anything, as Rakudo's
`List.EXISTS-POS(Int:D $pos)` does.
