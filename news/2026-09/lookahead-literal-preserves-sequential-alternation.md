# Lookahead literals preserve sequential alternation

Regex scanners now keep `||` visible when a quoted lookahead literal contains
nesting punctuation such as `(`.

Pinned by `t/regex/syntax/regex-lookahead-literal-seqalt.t`.

Closes #7912.
