# Grouped declarations keep `is default` when an initializer is present

`my ($a, $b) is default(7) = 1` now applies the group `is default` trait to every
element. Previously the parser dropped it whenever an initializer followed, so a
missing RHS value stayed `(Any)` and a later `Nil` assignment did not reset the
variable. The RakuAST conversion of a group default is still declined (unchanged).
