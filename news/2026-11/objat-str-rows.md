# `ObjAt.Str` is answered by the built-in method table

`.Str`, `.Stringy`, `.gist` and `.raku` on an `ObjAt` or `ValueObjAt` -- what
`.WHICH` returns -- had no row in the built-in method table, so each call fell
through the whole general method cascade: the user-shadowing query, MRO walks,
the native fallbacks, about 120k instructions before the identity text the
object carries was read.

Both classes are now opaque dispatch shapes whose rows are `ObjAt`'s, and the
four methods read the identity text directly. `$type.WHICH.Str` drops from
~186k to ~29k instructions per call (callgrind, release). A role mixed into an
`ObjAt` keeps the general dispatch, which answers the same text.
