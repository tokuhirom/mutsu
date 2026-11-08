# `only sub` / `only method` keep `multiness => "only"` in `.AST` and `.DEPARSE`

The parser treated `only` as a no-op word, so `"only sub f() { }".AST` lost the
`multiness => "only"` field rakudo's tree carries and `.DEPARSE` printed
`sub f { }`. The declarator is now recorded on the routine's source spans
(`only sub`, `our only sub`, `only method`, `only submethod`), rendered in the
RakuAST view and deparse text, and accepted when lowering a hand-built node
(#14845).
