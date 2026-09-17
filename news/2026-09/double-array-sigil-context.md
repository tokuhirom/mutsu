# `@@array` now reads the array instead of an anonymous empty array

The parser treated the first `@` in `@@items` as an anonymous array and left
`@items` as a discarded following term.  Rakudo treats the spelling as list
context applied to the named array.  The array-variable parser now consumes
the second sigil, so `@@items` has the same values and element count as the
ordinary array read.

Pinned by `t/lang/double-array-sigil-context.t`.  The bug was found in
PURL 0.0.15, where it made every `PURL::Type.examples` result empty.
