# RakuAST: a signature literal is `FakeSignature(Signature)`

`.AST` of `:(Int $x, Str :$y)`, `:(Int $a --> Bool)`, `:()`, a slurpy, defaulted, `where` or
callable parameter, and a literal used as an operand (`:(Int) ~~ :(Int $x)`) now renders rakudo's
`RakuAST::FakeSignature.new(RakuAST::Signature.new(parameters => (...)))`. Before, it was refused
as a `literal Instance`: the parser folds the literal to a `Signature` value, and the conversion
had no case for it.

No parser change was needed. The `Signature` value keeps the declared parameters it was built from
(`SigInfo::param_defs`), so the conversion reuses the signature builder a `sub` uses, and the
lowering turns the node back into the same value with `make_signature_value`.

14 more `t/` files pass under `MUTSU_RAKUAST=1` (the signature smartmatch, `Signature.ACCEPTS` and
nativecall signature tests). A `where *.foo` clause in such a literal is still lost on the round
trip (#12293). Remaining `literal Instance` refusals are a first-class `Label` and `X::Obsolete`.

This is a slice of the S10 residual work of #7564.
