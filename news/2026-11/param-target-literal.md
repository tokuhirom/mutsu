# Literal-value parameters carry `ParamTarget::Literal`

`ParamTarget` gained `Literal` (ADR-15761): the parser's `__literal__` placeholder now has a recorded
form like the other anonymous parameters. The duplicate-parameter check in `param_list.rs` and the
RakuAST literal-parameter view read `target` instead of comparing the placeholder name. The
precompilation cache format is bumped, as the serialized `ParamDef` can now hold the new variant.
