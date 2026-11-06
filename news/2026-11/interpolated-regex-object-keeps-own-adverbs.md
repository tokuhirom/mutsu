# An interpolated Regex object keeps its own adverbs under `<$re>`

`"A" ~~ m:i{<$re>}` with `my $re = regex { a+ }` matched: the `<$var>` arm parsed the object's
pattern with the enclosing literal's `:i` / `:m`. A Regex object now carries only its own adverbs
(an `rx:i{...}` object keeps its `:i`); a string interpolated the same way still inherits the
enclosing ones (#16921). The bare `$re` form next to an anchor (`m:i{^$re$}`) still inherits the
outer `:i`; that case is left on the issue.
