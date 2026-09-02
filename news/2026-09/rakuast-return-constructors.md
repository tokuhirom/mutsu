# Construct RakuAST return types

RakuAST construction now covers routine return-type nodes in addition to the
read-side and lowering support that already existed:

- `RakuAST::Signature.new(:parameters(()), :returns($type))` retains a
  `Signature.returns` node.
- `RakuAST::Trait::Returns.new($type)` and `RakuAST::Trait::Of.new($type)`
  construct the positional trait forms emitted by `.AST`.
- `RakuAST::Sub.new(:traits)` attaches those traits, allowing a hand-built
  routine to pass through the existing RakuAST lowerer and compiler under
  `EVAL`.

The implementation keeps the model-layer representation unchanged and is
pinned by `t/rakuast-construct-return-type.t`, which passes unchanged under
both mutsu and Rakudo.
