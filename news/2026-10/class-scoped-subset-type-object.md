# A class-scoped subset's type object satisfies its own subset

A `my subset` declared in a class body binds under a lexical-site key, so passing its type
object (`Dest`, spelled `Cat::Dest` internally) to a parameter typed with that subset ran the
`where` predicate against the type object and rejected it. A package value naming the subset
itself now matches the subset constraint directly, whichever spelling either side uses, and
`Associative[Dest]` no longer shows the site key. Found while running PDF's `t/cos-tie-entry.t`.
