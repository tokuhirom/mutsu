# `$match ~~ $match<quantified>` follows `List.ACCEPTS` for a non-Iterable topic

Smartmatching a non-Iterable topic against a List now follows Rakudo's
`List.ACCEPTS(List:D: $topic)`: the list answers itself when its first element
is a Match (the `m:g` smartmatch) and False otherwise. A multi method selected
by `$/ where $<x>` with a quantified `<x>*` therefore picks the `where`
candidate, as in CSS::Specification's `declaration` (#13251).
