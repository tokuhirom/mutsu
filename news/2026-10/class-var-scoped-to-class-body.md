# `$?CLASS` no longer leaks out of a class body

A class body bound `$?CLASS` in the scope enclosing it and left it there. So
the mainline answered the last declared class (`class A {}; class B {};
say ::('$?CLASS')` printed `(B)` where Rakudo fails), and the return merge
copied a callee's leaked `$?CLASS` back into its caller on every named
call. The class body now restores the outer binding when it finishes, and
the named-call return merge skips compile-time contextuals (`$?CLASS`,
`$?LINE`, ...), as the light call paths already did.

This was the largest of the categories the return merge still carries (599
of 730 test files). ADR-12529 phase 3 now lists the rest: frame metadata,
type names and by-name lexical writes. Each has to go before the merge can
be deleted.
