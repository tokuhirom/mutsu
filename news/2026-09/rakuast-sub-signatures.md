# RakuAST preserves positional sub-signatures

`sub f($x ($a, $b)) { $a + $b }` and array-destructuring parameters now render
their nested `RakuAST::Signature` instead of failing with
`non-trivial signature parameter`. The parser already retained the distinction
in `ParamDef.sub_signature`; this slice exposes it through the RakuAST model and
reuses the existing signature binder when lowering.

## Change

- `src/rakuast/convert.rs` recursively emits `Parameter.sub-signature` for
  ordinary positional and array-destructuring parameters.
- `src/rakuast/mod.rs` accepts and exposes `sub-signature` on
  `RakuAST::Parameter` construction and introspection.
- `src/rakuast/lower.rs` recursively rebuilds nested `ParamDef` values for
  `EVAL`, without introducing another execution path.

Named aliases, capture sub-signatures, type captures, and array-shape metadata
remain explicit follow-up boundaries.

## Coverage

`t/rakuast-subsig.t` pins the nested node and accessor shape, the implicit
`Type::Setting(Any)` on nested routine parameters, positional and array
destructuring EVAL round trips, and a hand-constructed nested signature that
lowers and executes. It passes verbatim under both mutsu and Rakudo.
