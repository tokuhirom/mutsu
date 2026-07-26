# RakuAST type objects expose implemented methods

`.^methods(:local)` on RakuAST type objects and node values now reports the
constructors and accessors implemented by mutsu's model layer.  The list stays
model-facing: Rakudo's compiler-private `IMPL-*` methods are not copied.

This completes RakuAST Phase 3 slice 8 and is covered by
`t/rakuast-type-methods.t`.
