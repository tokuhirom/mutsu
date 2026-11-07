# "Two terms in a row across lines" inside `( )` and `$( )`

#16975 made a missing `;` between two lines of a block a syntax error, but the
parenthesized forms stayed silent: `$(⏎ $a ** 3⏎ $a ** 2 ** 3⏎)` evaluated both
lines, and `(⏎ $a ⏎ $b ⏎)` failed with a generic parse error at the `(`. Both
now fail with `X::Syntax::Confused: Two terms in a row across lines (missing
semicolon or comma?)` when a plain term starts the next line with no `,` or `;`
before it. Text::CodeProcessing's `t/06` chunks (wrapped as `$( chunk )`) rely
on it.
