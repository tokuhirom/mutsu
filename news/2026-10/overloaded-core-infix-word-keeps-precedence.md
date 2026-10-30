# `multi infix:<cmp>` keeps the precedence of the core operator

Declaring a multi of a core infix word (`cmp`, `min`, `max`, `leg`, `x`, `xx`) made the parser take the word for a new operator with the trait-less additive level, so
`$b + 1 cmp $b + 1` read as `$b + (1 cmp $b) + 1`. The core operator's own level now parses it and
dispatches to the declared candidate. Pop's `t/Pop/Point.t` matches `raku`. The other core words
(`eq`, `lt`, `eqv`, `after`, `unicmp`, `coll`, `o`, ...) still go through the custom-word path when declared.
