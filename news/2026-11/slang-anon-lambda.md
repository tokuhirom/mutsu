# Slang::Mosdef runs: anonymous `λ ($n) { ... }` and memoized slang hooks

A slang that re-spells the stock `routine-declarator:sym<sub>` with keywords of its own and reads the
core definition after them with `key-origin` (Slang::Mosdef's `lambda` / `λ`) is now a production the
host runs, instead of being taken for Tuxic's no-op re-statement. The `routine-def` shim also reads the
anonymous forms (`λ ($n) { ... }`, `sub { }`), through `sub_after_keyword`, the function the term
parser's `sub` arm now calls. The per-unit hook answer table of ADR-15762 section 2.8 exists now: a slang
production nested in the block of another one was parsed once per enclosing attempt, so six nested `λ`
cost exponentially; they are linear. `t/01-basic.rakutest` and `t/02-y-combinator.rakutest` of
Slang::Mosdef are identical to `raku` (#13370).
