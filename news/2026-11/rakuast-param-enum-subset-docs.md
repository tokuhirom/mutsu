# Declarator docs on parameters, enums and subsets in `.AST`

`#|` / `#=` comments on a parameter (`sub f(#| the arg $x)`), an `enum` and a
`subset` are printed as a trailing `.declarator-docs(...)` call on the node in
`.AST` (#14880), like the docs of a sub, method or class: the pass that places
the parser's documented-declaration offsets on the converted tree now also
accepts `Parameter`, `Type::Enum` and `Type::Subset` nodes. A documented
`my $x` is still not placed (the parser does not record it).
