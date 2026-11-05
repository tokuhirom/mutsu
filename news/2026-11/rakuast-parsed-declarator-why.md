# `WHY` of a parsed RakuAST declaration

`Q[#| hello\n#| more\nsub f {} #= trail].AST.statements[0].expression.WHY` answered the
`Doc::Declarator` type object. It now answers a `Doc::Declarator` built from the declaration's
comments, with the declaration as `WHEREFORE`, as rakudo does; an undocumented declaration still
answers the type object, and a `WHY` given to a hand-built node still wins.

Rakudo keeps one list element per comment (`leading => ("hello", "more")`), but the parser joined the
comments of a declaration with a space. `DeclDoc` now also keeps the comments one by one
(`leading_lines` / `trailing_lines`), the hidden `declarator-docs-*` fields of a parsed node hold
them as lists, and `.raku` prints `.declarator-docs(...)` with one line per comment. The precompiled
cache format is bumped.

Not done (#16783 follow-ups filed): `EVAL($ast)` of a documented *named* sub loses its docs, a `my`
variable declaration gets no docs in the parsed AST, and `WHEREFORE` is a copy of the declaration
rather than the node itself.
