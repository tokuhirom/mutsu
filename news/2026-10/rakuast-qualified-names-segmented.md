# RakuAST: qualified names are segmented everywhere

A `::`-qualified name such as `A::B` used to stay one opaque
`Name.from-identifier("A::B")` string in most places. That covered a
`class` / `role` / `grammar` / `module` / `package` / `enum` / `subset`
declaration's own name, a qualified type used as a term or parent, and a
parameter type. Rakudo 2026.09 stores such a name as `Name::Part::Simple`
parts and prints it as `Name.from-identifier-parts("A","B")`.

`name_from_identifier` now does the segmenting itself, so every caller in the
converter agrees with Rakudo and `.parts` is walkable. A name that only
happens to contain `::`, such as the operator name `infix:<::=>`, stays one
identifier. The lowerer already reads names through `name_parts::name_shape`,
so qualified declarations, parents and grammars still round-trip through
`.AST.EVAL`.

Regression test: `t/rakuast/rakuast-qualified-decl-name.t` (GH #10655).
