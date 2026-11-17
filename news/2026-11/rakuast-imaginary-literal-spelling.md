# RakuAST: an imaginary literal keeps the number it was written with

The parser folds an imaginary literal (`0.0i`, `2.5i`, `3e2i`) to its Complex
value. The RakuAST view then had to guess the number back from the imaginary
part, so `0.0i` came back as `0i` and `1.5e0i` as `1.5i`. Now the parser also
remembers the number as written (`Spelling::Imaginary`). The view keeps it as
the literal operand of a `Postfix("i")`, as rakudo's RakuAST frontend does:

- `0.0i` is a `RatLiteral` and deparses as `0.0i`;
- `3e2i` is a `NumLiteral` (`300e0i`);
- `2i` is an `IntLiteral`.

The value is unchanged. The view code moved to `src/rakuast/imaginary_literal.rs`.

`roast/S32-trig/cosech.t` and `tanh.t` now match rakudo statement for
statement in both corpora.
