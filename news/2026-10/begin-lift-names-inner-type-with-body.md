# A nested BEGIN naming a lexical class whose body runs code is lifted

A BEGIN nested in a routine that names a `my class` declared ahead of it in the same scope was
left on its in-place handling whenever the class body ran code (`say`, an `our $x = ...`), because
repeating the declaration in the BEGIN's block would have run that code at BEGIN time. The
lifted block now repeats a declaration-only shell of such a class, which registers the same
lexical type the in-place declaration later completes and runs the body for real (#10471).
A variable typed by an inner type, a class with a user trait and a nested type that itself runs code are not covered yet.
