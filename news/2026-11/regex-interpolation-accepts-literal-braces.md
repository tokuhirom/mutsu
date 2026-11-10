# `<$re>` accepts a `{` or `}` that is only a character

Interpolating a string with `<$re>` refused any pattern that contained a brace
("Prohibited regex interpolation"). Only a brace that opens a code block is
refused now; a single-quoted brace (`'{'`), a backslash-escaped one (`\{`) and one
inside a `<[...]>` character class are literal characters and are accepted, as in
rakudo. A quote or class left unterminated is still answered conservatively, so a
malformed pattern cannot hide a code block behind a stray quote.

Found by running the LIVR distribution (`t/09-like-string-regexes.t`: the `like`
rule translates `a{,3}` to `'a' '{' ',' '3' '}'`).
