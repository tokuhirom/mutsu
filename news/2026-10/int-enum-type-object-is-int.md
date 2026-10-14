# An Int-based enum type object is an `Int` type

`enum E <a b>; E ~~ Int` was False and `sub i(Int $x)` rejected `E`. The enum type object
now matches `Int`, and multi ranking puts it one step below `Int`, so `multi mt(E)` beats
`multi mt(Int:U)` for `mt(E)` as in Rakudo (#13243).
