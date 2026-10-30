# A carried body that declares no routine takes no lexical scope of its own

When the executing round trip lowers a carried tree, each statement body used to push a parser
lexical scope so that a `sub` or `my &name` declared in it would be in scope for the statements
after it. Most bodies declare neither, so the scope is only pushed when one does.

Frontend Ir against the frozen baseline (profiling build, callgrind): zef-cli 108.3% -> 108.2%,
s12-class-attributes 106.0% -> 105.8%, grammar-cursor-attributes 112.9% -> 112.5%; the other
two cases are unchanged. #15657 stays open for the grammar case; its attribution is in the
issue.
