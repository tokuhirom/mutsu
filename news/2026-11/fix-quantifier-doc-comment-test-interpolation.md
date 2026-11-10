# A regex test no longer interpolates `{x}` in a test description

`t/regex/syntax/regex-quantifier-after-doc-comment-branch-start.t` had a double-quoted description with an
unescaped `{x}`, which rakudo rejects as an undeclared routine. It is escaped, so the file now runs identically
under `raku`, mutsu and as `EVAL(slurp($file).AST)` (registered in the ratchet, #16356).
