# A nested BEGIN can name a role whose body runs code, or a class with such a nested type

`sub f { my role R { say "body" }; BEGIN say R.raku }` now runs its BEGIN at
compile time even when `f` is never called. A role's body runs when the role is
composed, not when it is declared, so the lifted BEGIN's block can declare the
role again without running any of it. A class whose nested class runs code is
declared again as a shell of both levels, so a BEGIN naming it is lifted too
(#10471).
