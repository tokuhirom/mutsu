# `$=pod` and `$=finish` are `RakuAST::Var::Doc`

`.AST` of `$=pod` or `$=finish` now answers a `RakuAST::Var::Doc` holding the
name after `$=` (#14880), as rakudo does, instead of a `Var::Lexical` named
`$=pod`. The class has the positional constructor, the `name` accessor and
rakudo's `.raku`, and a hand-built `Var::Doc` lowers to the Pod variable.
