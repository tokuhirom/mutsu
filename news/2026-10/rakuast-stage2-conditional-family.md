# The parser builds `if`/`unless`/`with`/`without` in their RakuAST shape

The conditional family is the first construct family on ADR-10723 Stage 2. It
uses the mechanism proposed in the ADR's §4 amendment (#14652). The parser now
builds `if`, `unless`, `with` and `without`, together with their `elsif`,
`orwith` and `else` clauses, as a typed `ast::parsed::Conditional`. The node
keeps the condition as written (no `!` for `unless`, no `.defined` test for
`with`), each clause's block and pointy parameters, and the source ranges Rakudo
gives each `Block` / `PointyBlock`, its `Blockoid` and its `StatementList`.

An executing parse lowers the node where it is built, so the compiler and every
analysis still see the same `Stmt::If`. The desugaring the parser used to do
inline now lives in one function, `rakuast::conditional_lower`. Lowering a
hand-built `RakuAST::Statement::If` reads the node into the same struct. Before
this change it used its own copy of the `with`-block scaffold (`with_desugar::with_conditional`,
`control_signature::with_body`). That copy is gone. A `.AST` parse keeps the node in `Stmt::Ast` and renders it.

What `.AST` users see:

- `"if 1 \{ 2 } else \{\n 3\n}".AST` blocks have origins, so the line of an
  `else` is visible to `visit-children` walkers like `Code::Coverable` (#11413).
  The ranges match rakudo 2026.09, pointy blocks included (`PointyBlock` from
  the `->`, `Blockoid` from the `{`).
- `visit-children` of a conditional visits each `elsif`/`orwith` clause's
  condition and block directly, as rakudo does, not the clause node.
- An empty `else {}` no longer renders the `Nil` statement the desugaring added.

Pinned by `t/rakuast/rakuast-origin-conditional.t`, which passes on both mutsu
and rakudo.
