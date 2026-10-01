# `ENTER` embedded in an expression runs at routine entry

`sub timer(&c) { &c(), now - ENTER now }` produced a negative elapsed time because the `ENTER now`
operand was evaluated in place, after the left-hand `now`. Single-expression `ENTER` operands inside
sub and closure bodies (binary, unary, postfix, grouped and comma-list positions) are now hoisted
into an entry-time phaser, as loop bodies already did. Found via the `Timer` distribution; its
`t/01-basic.t` now passes.
