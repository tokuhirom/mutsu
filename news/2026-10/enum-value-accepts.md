# An enum member's `.ACCEPTS` compares by value

`Less.ACCEPTS(-1)` was `False`: the enum dispatcher's `ACCEPTS` arm compared enum identity only, while an enum member is its
base type's value and answers that type's `ACCEPTS` (`Int`'s for `Order`, `Str`'s for a `Str`-valued member). It now answers
the smartmatch as the `~~` operator decides it (`method_table::accepts_by_smartmatch`, the pure comparison first, then the
interpreter's), which the `ACCEPTS` rows of the open shapes use too (#13333).
