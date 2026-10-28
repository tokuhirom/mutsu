# `anon`, `::` and `class ::Name` are a field of the package declaration

`Stmt::ClassDecl` and `Stmt::RoleDecl` gain `marks: PackageMarks { anon, colons, leading_colons }`.
The `__anon_decl`, `__anon_colons` and `__leading_colons` entries of `custom_traits` are gone, with
their constants, `convert::{is_anon_package, is_colons_package, has_leading_colons}` and the
`custom_traits` exemptions that skipped them; the compiler's redeclaration check and the nested-type
hoisting read `marks.anon` directly.
