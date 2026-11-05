# A junction or list in a regex `$var` is read at match time

A `$var` holding a junction (an alternation of its eigenstates) or a list (matched
as its string) is now read when the cursor reaches it, like a string, number or
`Regex` value, instead of being spliced into the pattern text first. A callable,
a `constant` (inlined at compile time, as rakudo does) and the atom of a
`$<a>=$re` alias still take the splice path.
