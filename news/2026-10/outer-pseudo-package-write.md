# Writes through `$OUTER::x` reach the outer lexical

A bind through the `OUTER::` pseudo-package now aliases the outer variable:
`my $x = 1; sub s { my $y; $OUTER::x := $y; $y = 5 }; s(); say $x` prints `5`,
as in Rakudo, where mutsu used to print `1`. The `$OUTER::x := my $y` spelling,
which used to die with "assign requires a concrete object" on the next
`$y = ...`, works too (#10676, #10675).

Reads of `$OUTER::x` were already resolved lexically by the compiler, but every
write — `:=`, `=`, `++`/`--`, compound assignment, and the `@OUTER::a` /
`%OUTER::h` forms — was stored under a literal `OUTER::x` key that nothing
read back. When `OUTER::` names the same binding an unqualified `$x` sees from
the write site (`lex_scope::outer_is_visible_binding`), the compiler now
compiles the write as a write to `$x`, so the ordinary rebind machinery keeps
the declaring slot, captured cells and the env coherent.

A write to an outer `$x` that an intervening scope shadows is still lost; that
needs a depth-addressed write and is tracked as #10827.
