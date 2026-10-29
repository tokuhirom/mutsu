# `RakuAST::Node.DEPARSE`

`.DEPARSE(*@roles)` prints a RakuAST tree back as Raku source by running
rakudo's own `RakuAST::Deparse` (ADR-14845): the 2026.09 file is vendored
verbatim in `vendor/rakudo/` (a sparse tree; `vendor.lock` row `rakudo`,
`scripts/update-vendor.sh` learned the seventh `paths` column) and is injected
as a prelude into a program that mentions `DEPARSE` or `RakuAST::Deparse`, so a
program that never deparses pays one substring search. A `Str` role names a
language (`DEPARSE("BG")`), a role is mixed in, a class replaces the deparser,
as in rakudo. Output matches `raku` on every probe tried (statements,
signatures, classes, loops, `given`, regexes, string interpolation, L10N
roles); `L10N::BG` `t/01-bulgarian.t` now passes 16 of 16.

What the vendored deparser needed from mutsu, all fixed in mutsu:

- `.AST` converts string-literal return types with escapes (`--> '[\\'`,
  `--> "\{\n"`) and lowers them back;
- `Class.^mixin(Role)` keeps the class's `proto method` body (the deparser sets
  its dynamics there);
- `my $*X := V` after the same frame read an outer, immutably bound `$*X` makes
  a fresh binding instead of assigning into the outer cell;
- `VarDeclaration::Signature.type` and `Package.is-stub` accessors.

Closes #15378.
