# A rule name with an apostrophe keeps its regex source tree

`/<with'hyphen>/` called a rule whose name joins its parts with an apostrophe,
but the regex tree scanner read the `'` as the start of a quote and gave up, so
the literal had no source tree and `.AST` of the whole unit failed with
"literal Regex without a declarator tree". A `'` between identifier characters
now joins the name (in both the scanner and the simple-name check), so such a
regex round-trips through RakuAST and `EVAL($ast)` matches raku. A quote after
any other character opens a quote as before.
