# `mod=`, `div=` and `gcd=` on an undefined target die

A compound assignment to an undefined variable starts from the operator's zero-argument value.
`mod` and `div` have no zero-argument candidate ("Too few positionals passed; expected 2
arguments but got 0") and `gcd` has no zero-argument meaning, but mutsu read the undefined value as
0 and carried on. `MetaAssignIdentity` gains `TooFewPositionals` and `NoZeroArgGcd`, and the word
forms (`mod=`, `div=`, `gcd=`, in a declaration or on an existing variable) seed their target with
them.
