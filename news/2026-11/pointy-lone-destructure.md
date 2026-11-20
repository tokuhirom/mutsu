# `-> ($a)` destructures, and `-> ($a, $b), $c` keeps its group

A parenthesised pointy-block signature with one element, `-> ($a)`, used to be
folded into a plain `-> $a`, so `-> ($x) { }(21)` bound `21` instead of dying,
and a group followed by more parameters (`-> ($a, $b), $c`) was not read as a
sub-signature at all. Both now parse as rakudo does: the group is one
destructuring parameter, a value without a `Capture` (an `Int`, `Str`, `Num`,
`Bool`, `*`) raises `X::Cannot::Capture` ("Cannot unpack or Capture `21`."),
and the RakuAST view of such a block carries the sub-signature (#18397).
