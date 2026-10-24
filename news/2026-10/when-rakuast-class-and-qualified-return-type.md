# `when RakuAST::Literal`, and a lower-case module's type as a return type

Two parse/registration gaps found by the `static` distribution (`static::Predicates` did not load):

- `when RakuAST::Literal { ... }` raised "needs parens to avoid gobbling block": the parser knew only
  a few RakuAST names as terms. Every RakuAST class the CORE setting declares (`core_type_names.txt`)
  is now a complete term. `RakuAST::Literal` and `RakuAST::Termish` are real abstract classes in the
  model hierarchy, so `IntLiteral.^mro` is `IntLiteral Literal Term Termish Expression Node Any Mu`
  as in rakudo, and `$int-literal ~~ RakuAST::Literal` holds.
- A routine declared `--> Fact` with `Fact` imported from a lower-case-named module
  (`static::Types::Fact` once resolved) was classified as returning a *value*, so a `return EXPR` in
  its body was rejected at load, or silently returned `Nil`. A package-qualified name is a type.
