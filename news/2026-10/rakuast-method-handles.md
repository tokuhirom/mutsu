# `method … handles` round-trips through RakuAST

`.AST` refused a method with a `handles` trait (`method build handles<greet>
{ Helper }`, `method inner() handles * { … }`). It now renders it as rakudo
2026.09 does, `Method(traits => (Trait::Handles(TERM),))` with the term as
written, and `EVAL` of such a tree installs the same forwarders as the parsed
declaration.

The written term is kept in the routine's source-trait record
(`RoutineTrait::Handles`), next to its `is`/`returns`/`of` traits and in the
same written order, rather than in a new `MethodDecl` field. Lowering rebuilds
the delegation specs from it with `parser::handle_specs_from_term`, the same
routine that reads an attribute's `handles` term.

`t/rakuast/rakuast-method-handles.t` pins the tree, the EVAL direction and the
parsed semantics; with the three `t/oo/trait/` files whose first refusal this
was, the round-trip ratchet grows by four files.
