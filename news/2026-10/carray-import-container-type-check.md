# `my CArray[T]` in a module sub no longer dies when `CArray` sits in a container

`has_type` resolved an imported type alias only when the env held a bare `Package`.
After a routine of the module used `CArray[Str].new`, the imported `CArray` term was
a container cell, so `my CArray[N-GError] $e .= new(N-GError)` died with
"Type 'CArray[N-GError]' is not declared." (Gnome::Glib::Variant `.new(:parse)`).
The alias is now dereferenced first. Gnome::Glib's `t/Variant.rakutest` and
`t/VariantDict.rakutest` now pass.
