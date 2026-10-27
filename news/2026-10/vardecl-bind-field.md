# Binding declarations are a typed field of `VarDecl`

`Stmt::VarDecl` now carries `bind: Option<BindKind>` instead of the `__scalar_bind` and `__code_bind`
marker traits. The parser, the RakuAST lowering, the destructuring desugar and the compiler's
synthesized binds set the field, and the compiler, the BEGIN lifting and TRIR read it. See ADR-10723
section 6.124.
