# RakuAST: the `Doc::Declarator` surface (`declarator-docs`, `cut-WHY`, mutators, visited child)

`RakuAST::Doc::Declarator` now has its builders: `add-leading`, `add-trailing`, `set-leading`,
`set-trailing` and `paragraphs`, and can be built without a `WHEREFORE`. A documented declaration
answers `declarator-docs(:@leading, :@trailing)` (the call `.AST` prints after it, now runnable: it
replaces both texts and answers the invocant) and `cut-WHY` (answers the documentation and removes
it). `Doc::Declarator.new(:WHEREFORE($sub))` given to `$sub.set-WHY(...)` no longer makes the tree
contain itself (it overflowed the stack on `EVAL`), and `EVAL` of a hand-built documented
declaration gives the routine its `.WHY`.

`visit-children` of a documented declaration visits its `Doc::Declarator` where rakudo does: after
the name of a routine (first when anonymous), before the traits of an `enum`/`subset`, last for a
class, role, grammar, module, package, variable or parameter. Measured against rakudo 2026.09 on
`t/rakuast/rakuast-doc-declarator-surface.t`. Closes #17047.
