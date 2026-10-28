# `(my $c := $obj.attr)` aliases an `is rw` attribute

A scalar bind written inside an expression (`if !(my $comm := $func.commute) { $comm = ... }`)
copied the attribute's value, so the later assignment died "assign requires a concrete object". It now
flags the accessor call as the statement form does and aliases the attribute container. Found
loading Math::Symbolic, whose test file now matches `raku`.
