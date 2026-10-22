# RakuAST round-trips numeric literal parameters

Routine, pointy-block, and signature-literal parameters now expose Complex values
through `RakuAST::Parameter` with the matching `Complex` type. Large rational
literals retain their `Rat` type across the same conversion, including values
stored in mutsu's wider rational representation. Pointy blocks now parse
numeric literal parameters through the shared signature grammar, so signed
Complex values round-trip and dispatch by value as they do under Rakudo.

`t/rakuast/rakuast-complex-literal-parameters.t` checks the node fields and
execution after lowering. The previously interrupted
`roast/S06-signature/types.t` passes with the RakuAST frontend.
