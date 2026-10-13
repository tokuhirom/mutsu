# A nested BEGIN can name a role whose body runs code

`sub f { my role R { say "body" }; BEGIN say R.raku }` now runs its BEGIN at
compile time even when `f` is never called. A role's body runs when the role is
composed, not when it is declared, so the lifted BEGIN's block can declare the
role again without running any of it (#10471).
