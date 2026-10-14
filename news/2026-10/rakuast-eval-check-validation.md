# `RakuAST::Node.EVAL` runs the EVAL unit's CHECK-time validation

`EVAL` of a RakuAST tree now validates the lowered unit the way a string
`EVAL` validates its parse (#13295): an undeclared variable, name or routine
is a compile-time error before any of the tree runs, and declaring the same
package twice in one process is `X::Redeclaration`, as in rakudo.

Turning it on exposed false positives in the validation that a string `EVAL`
shared, all fixed:

- `constant term:<$bar> = 42; $bar` — a sigiled term constant declares `$bar`;
- `<value:sym<number>>` — the `:sym<…>` subrule adverb is no term;
- `use M :Tag; C` / `import M :Tag; C` — a `use`d module's enum values and
  value terms, read from the parser's module-scan cache;
- `use A::Greeter; Greeter` — a qualified type exported under its short name;
- `module N { our $v is export }; import N; $v` — exported variables of a
  package the unit declares;
- `(my $a, my $b) »=» (1, 2); $a` — a desugaring's block is no scope.

A `use` whose imports come from a `sub EXPORT` hook is still only
approximated (lowercase calls are not judged); the real fix, running the
unit's `use` statements before the check as rakudo does, is #13441. The
round-trip test helpers that EVAL'd the same declarations twice now compare
the round trip with the expected value only.
