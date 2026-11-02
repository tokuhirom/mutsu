# Enum coercion calls work when the exporting module's `unit module` name differs from its file

`use C10; Errno(2)` died with `Unknown function: Errno` when `C10.rakumod`
declared `unit module Other::Name; enum Errno is export (...)`. The importer's
block-scoped binding carried the type object, but the by-name coercion call
only found the enum through a same-named package leak. An exported enum type
now records its short name in the importer's type aliases
(`alias_exported_enum_type`), so `Errno(2)` resolves as in Rakudo (#16041).
