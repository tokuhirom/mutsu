# Smartmatch against a `does Real` instance compares numerically

`3 ~~ R.new(n => 3)`, `R.new(n => 3) ~~ 3`, `R.new(n => 3).ACCEPTS(3)` and
`R.new(n => 3) ~~ R.new(n => 3)` were all `False` for a user class composing `Real`; they are
Rakudo's `Numeric.ACCEPTS`, `a == self`, and now compare through the class's `Bridge` on either
side (#13894). The instance's class MRO walk finds `Numeric.ACCEPTS` ahead of `Any`'s identity,
and the smartmatch no longer reads such a matcher by identity.
