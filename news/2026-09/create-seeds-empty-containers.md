# CREATE seeds empty containers for @/% attributes

`self.CREATE!SET-SELF(...)` on a class with `has int @!from` used to seed the
attribute with its element type's default (`0`), so the first `.push` died with
"No matching candidates for method: push". `CREATE` now seeds every `@`
attribute with an empty (typed) Array and every `%` attribute with an empty
Hash, freshly allocated per instance. Found via the `String::Fields`
ecosystem distribution; its remaining failure (an `is rw` typed parameter's
constraint leaking into assignment) is tracked separately.
