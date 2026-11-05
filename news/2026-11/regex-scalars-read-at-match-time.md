# Every plain `$var` of a regex is read at match time

A pattern without a code block used to splice each `$var`'s value into the pattern text before
the match, so every distinct value parsed and compiled its own pattern. A plain scalar holding
a string, number, `Nil` or a `Regex` is now declared as a match-time lexical for any pattern
that names it, and lowered to a `RegexAtom::VarInterp` that reads the value when the cursor
reaches it (ADR-12917, direction 2; #14510).
