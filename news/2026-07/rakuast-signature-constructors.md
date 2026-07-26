# Construct plain RakuAST signatures

RakuAST construction now covers the plain positional signature model:
`ParameterTarget::Var.new`, `Parameter.new`, and `Signature.new`. Constructed signatures are
walkable through their normal accessors, render in Rakudo's constructor form, and can be attached to
`Sub.new(:signature)` and lowered through the existing compiler by `EVAL`.

The implementation validates each child node at the model boundary, defaults an omitted parameter
list to an empty list, and exposes the new constructors and fields through `.^methods(:local)` and
`.^attributes(:local)`. The regression test runs unchanged under both mutsu and Rakudo.
