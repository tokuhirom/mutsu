# A declared name with an integer-list colonpair round-trips through `.AST`

`my $y:a[1,2] = 3; $y:a<1 2>` now runs through `.AST.EVAL`: the RakuAST view
spells a colonpair value holding integers (and any other expression item)
back, instead of raising "a variable name whose colonpairs cannot be
spelled". `Name.canonicalize` and `.DEPARSE` give the canonical `y:a<1 2>` as
rakudo does. A plain `EVAL` of `my $y:a(1) = 3; $y:a<1>` no longer reports the
variable undeclared either: the `EVAL` pre-scan now reads a BEGIN-evaluated
colonpair value the way the compiler canonicalizes it (#18612).
