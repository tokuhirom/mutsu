# `return X and Y` keeps its tail in the tree; `0 or return 1, 2` returns the list

`return 1 and 2` is `(return 1) and 2`: the loose word-logical applies to the
`return` call, whose right side never runs. The parser used to discard the
tail, so `.AST` / `.DEPARSE` read back `return 1`; the statement is now the
infix over a `return` call in expression position, as in rakudo 2026.11's
tree (`return 1 xor 2` keeps its `xor` spelling), and the right side still
never runs (#18302).

That expression form exposed a run-time bug: `return` as a listop in
expression position (`0 or return 1, 2`) returned only its first argument;
it returns the whole list now, like the statement form.
