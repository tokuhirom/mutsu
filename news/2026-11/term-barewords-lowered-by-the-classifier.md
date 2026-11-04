# Plain term barewords are lowered by the classifier's answer

A sigilless declaration (`my \s`), a lowercase `constant` or an imported value
used as a bareword were converted to RakuAST and lowered back by the executing
finalization (the second largest class of bareword conversions in the census of
700 `t/` files). When the classifier (`bareword::view_node`) resolves a plain
identifier to `Term::Name`, the lowering is `term_identifier_expr` over the
word (`True`, `Empty`, `pi`, `tau`, `e` unless shadowed, otherwise the word), so
`bareword::term_expr` returns that without building the node tree (ADR-15760
slice 2). Only an unspelled word qualifies: a spelling (an imported value, a label term)
tells the conversion how the parse resolved the word, which the classifier alone
does not know, so those, a qualified name and a label in scope are still resolved
by the conversion. `.AST` and `EVAL($ast)` are
unchanged.

A word the finalization already carries as a type name (capitalised: `True`, `X`)
is left to the compiler, so a local `constant True = 42` still wins.
