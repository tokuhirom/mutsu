# RakuAST: a `;` that ends no statement inside a `unit` declaration has its range

In `unit module M;` / `unit class C;` (and role, grammar), a `;` that ends no statement in the rest
of the unit is a `Statement::Empty` with the range `raku` 2026.09 gives it (`unit module EmB;\nsub f { 1 }\n;;\n`: 30-30).
mutsu listed the node but without a range: the statement being converted was the header's range only,
so the term's origin was not found. The conversion now enters the statement widened over the unit it holds.
Test: `t/rakuast/rakuast-unit-empty-statement-origins.t`.
