# A subclass `multi method` no longer falls back to the parent's `only`

When a class declares `multi method m` and its parent declares a plain
`method m`, the child's implicit proto dispatches over multi candidates only.
mutsu walked on to the parent's `only` method when no child candidate bound,
so `Child.m(1)` answered `'parent'` where rakudo dies with
`X::Multi::NoMatch`.

Both method resolvers (the live MRO walk and the resolved-sequence ranker) now
skip an ancestor level that declares no multi once a more-derived level
declared multis. Ancestor multis further up stay candidates, as in rakudo,
which derives the child's proto from the nearest ancestor dispatcher and never
from an `only` (#17572; found on the Red ORM suite).
