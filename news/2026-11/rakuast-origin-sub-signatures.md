# RakuAST origins of sub-signatures in pointy blocks and regex declarations

A sub-signature written as a parameter of a pointy block, a regex declaration
or a `my (...)` declaration (`-> Int $x, ($a, :$b) { }`, `token w(($a, $b))`)
is now placed as rakudo 2026.11 places it: the parameter over its
parentheses, its `Signature` over the text inside them, and each inner
parameter over its own text. The whole signature used to get no ranges
(#15780).

Found on the way: `-> ($a) { }` is read as `-> $a { }`, a lone positional
sub-signature that does not destructure (#18397).
