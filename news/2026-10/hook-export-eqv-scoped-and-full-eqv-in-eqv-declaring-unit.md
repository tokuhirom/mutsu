# `eqv` in a unit that declares its own `infix:<eqv>`, and a hook-exported `&infix:<eqv>`

Two gaps made Red's `t/06-optimizer.rakutest` answer `Red::AST::OR.optimize(C.a, not (C.a), 1)`
with the unreduced node instead of `Value True` (#15203):

- The `&infix:<eqv>` family a `sub EXPORT` hook (Red's `Red::Operators::EXPORT::ALL::`) installs
  is now scoped to the unit that imported it. A role module that never imported the operator
  used to find the importer's family through the shared `env`, so its own `eqv` (and the `eqv` in
  a multi candidate's `where` clause, which runs in the declaring unit before the candidate has a
  frame) resolved to `Red::AST::Eq` objects instead of core `eqv`.
- A unit that declares an `infix:<eqv>` candidate compiles the operator through the generic infix
  route, which fell back to the pure structural `Value::eqv`. It now goes through the same
  `eqv_values` as `OpCode::Eqv`, so rakudo's rule (identical, or same type and equal `.raku`)
  holds there too.
