# `also is Head::Sub::Gram` no longer resolves to `Any` once `use Head` ran

Once a module granting the top package `Head` was loaded (`use CSS::Module` inside a method body),
the type-alias walk read the qualified parent name `CSS::Module::CSS3::Fonts::AtFontFace::Gen::Grammar`
through the unmerged-package-variable store, which answers `(Any)` for a name nobody stored. The walk
took that for an alias to `Any`, so the class got `Any` as a parent instead of the grammar, and the C3
merge died with "Inconsistent class hierarchy" (or the role-requirement check with "Method ... must be
implemented"). A type name is never a package variable; the walk no longer asks the variable store.
This was the single root cause of five `blocked_load` ecosystem records (CSS::Module, CSS::Properties,
CSS::Font::Resources, CSS::Stylesheet, ...); CSS::Module's own suite goes from not loading to 10 of 16
files passing.
