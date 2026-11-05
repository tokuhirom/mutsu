# `Pkg::Name (args)` for a package that is not loaded is a call

`SDL::GetTextureBlendMode ($!texture, $mode) and die ...` (Pop's `Drawable` role) died with
"Confused. Two terms in a row": a package-qualified name whose last component is capitalised
was always read as a type, so the following spaced `(` could not start its argument list. The
name is now a call when no known or declared type matches it or its package, as in raku; a
known type keeps `Int (elem) @types` an infix, and the set-operator spellings never count as an
argument list.
