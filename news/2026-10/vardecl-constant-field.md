# `constant` declarations are a typed field of `VarDecl`

`Stmt::VarDecl` now carries `constant: Option<ConstantSigil>` instead of the `__constant` and
`__constant_sigil` marker traits. The parser, the RakuAST lowering and about forty readers in the
compiler, runtime and `convert` use the field, and the sigil recovery by trait scan in `convert` is gone.
See ADR-10723 section 6.123.
