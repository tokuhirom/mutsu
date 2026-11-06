# An interpolated Regex object keeps its own adverbs under `<$re>`

`"A" ~~ m:i{<$re>}` with `my $re = regex { a+ }` matched: the `<$var>` arm parsed the object's
pattern with the enclosing literal's `:i` / `:m`. A Regex object now carries only its own adverbs
(an `rx:i{...}` object keeps its `:i`); a string interpolated the same way still inherits the
enclosing ones (#16921). The bare `$re` form (`m:i{^$re$}`) is fixed the same way: a Regex
value no longer forces the case-folding match of the whole subject, which had turned the object's
exact-case literals into folded ones.
