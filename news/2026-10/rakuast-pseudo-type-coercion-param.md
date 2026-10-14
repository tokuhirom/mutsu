# RakuAST: `::?CLASS() $x` parameters

A coercion over a pseudo-type (`::?CLASS()`, `::?ROLE()`) in a signature was
mistaken for a `::T` type capture and refused. It now renders as a
`Type::Coercion` over `Type::Simple("::?CLASS")`, as rakudo does, and lowers
back unchanged. One more `t/` file passes under `MUTSU_RAKUAST=1`
(ADR-10723 S10).
