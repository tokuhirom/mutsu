# The declarator keyword of a package is a field

`PackageMarks` (on `Stmt::ClassDecl` and `Stmt::RoleDecl`) gains `declarator: Option<String>`: the
keyword a registered `EXPORTHOW::DECLARE` declarator (`monitor Name { }`), or a built-in one whose
HOW `EXPORTHOW::SUPERSEDE` replaced, declared the package with. The `__mutsu_declare_how` entry of
`custom_traits` is gone, with its constant, `declare_how_marker`, `declare_how_keyword`,
`supersede_marker` (now `superseded_declarator`, returning the keyword), `PackageHead::custom_traits`
and the trait-list filters in `convert` and `role` that skipped it; the class and role plans carry the
keyword for registration.
