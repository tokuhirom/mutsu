# RakuAST: package-qualified variables are `Var::Package`

A package-qualified variable such as `$Foo::v`, `@A::B::c`, `%GLOBAL::h` or
`&CORE::uc` now renders as the `RakuAST::Var::Package` node Rakudo 2026.09
produces. That node holds the segmented `Name` and a `sigil` field. mutsu used
to render the whole spelling as one `Var::Lexical("$Foo::v")`.

The new node works in both directions:

- It is constructible with `RakuAST::Var::Package.new(name => ..., sigil => ...)`.
- It is a `RakuAST::Var` and a `RakuAST::Term`.
- It lowers back to the qualified variable the parser produces, also as the
  target of an assignment, so `.AST.EVAL` round-trips for every sigil.

The regression test is `t/rakuast/rakuast-var-package.t` (GH #10653).
