# `@$x` and `%$h` are contextualizers over the bare variable in `.AST`

`@$x` was shown as `Contextualizer::List(Circumfix::Parentheses(...))` (the parser keeps the scalar
`Grouped` for the subscript-assign path) and `%$h` as a written `$h.hash` call. rakudo's tree holds
`Contextualizer::List` / `Contextualizer::Hash` over the bare `Var::Lexical`. Both now match, so
`.DEPARSE` writes `@$x.push(4)` and `%$h.elems` back as written, and `EVAL($tree)` rebuilds the
same re-reading contextualizers (#14845).
