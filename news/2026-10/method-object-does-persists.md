# `does` on a method object persists across lookups

`$m does R` on a method object from `.^find_method` / `.^lookup` now composes the
role into the method's declaration (its `MethodDef` routine cell), so later
lookups of the same method carry the role. The object carries the owner, name and
candidate index of its declaration, and `does` writes the composition through to
that cell (#12287).
