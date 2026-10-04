# A role or class that shadows its own parent's name inherits the outer type

While a role or class is being declared, its own name is not yet visible to its
traits. So SQL::Abstract's `role Exception is ::Exception` and plain
`role Exception is Exception` inherit the core `Exception`, as rakudo does. A
top-level role used to resolve the parent to itself and drop it, and a
top-level `class Exception is ::Exception` died with "cannot inherit from
itself". Such a parent is now kept as the core type the declaration shadows.

A role that a class inside a `module` composes with `does` also no longer shows
up in that class's `.^mro`. The class recorded the role under the name it was
written with (`R`) while its parents held the package-qualified one (`M::R`), so
the composition was read as an `is R` pun (#11072).
