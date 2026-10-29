# A declaration with sunk list items is one typed comma list

ADR-15334: `my $x = 1, 2` (and the trailing-comma form `my $x = 1,;`, the sigiled forms and the
generic fallback in `my_decl_assign`) no longer build a `SyntheticBlock` in the parser. All of them
are `ParsedStmt::CommaDecls`, whose single lowering expands the declaration and the items. `.AST`
of `my $x = 1,;` used to fail with an unsupported-construct error and now matches rakudo.
