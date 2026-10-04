# RakuAST: imaginary literals

`2i`, `3.5i` and `1+2i` were refused as "literal Complex(…)" in 33 `t/` files.
mutsu's parser folds an imaginary literal straight to its Complex value. Rakudo
2026.09 keeps the number and applies the `i` postfix to it:
`ApplyPostfix(IntLiteral(2), Postfix("i"))`. For `1+2i` that node is the right
operand of a plain `+`.

A literal Complex with a zero real part now renders this way, and the
lowering folds the postfix back to the same Complex. The parser keeps only
the value, so the converter picks the number's spelling from it:

- an integral imaginary part is an `IntLiteral` (`2i`);
- any other finite part is the `RatLiteral` of its shortest decimal spelling
  (`3.5i`).

So `3.5e0i` comes back as `3.5i`, which denotes the same Complex. Making the
parser keep the spelling would need a `LiteralSrc` in primary position, and
the parser deliberately keeps those out of there.

A Complex with a non-zero real part, such as the angle literal `<1+2i>`
(rakudo's `ComplexLiteral`), stays refused.
