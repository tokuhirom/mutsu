# RakuAST: ranges of `unit class`, `unit role` and `unit grammar`

`unit class C;`, `unit role R;` and `unit grammar G;` hold the rest of the unit as their body.
Their ranges now match `raku` 2026.09: the unit's list, the statement and the declaration run to
the end of the unit's last statement (terminators included), the name has its range, and the body
(block, blockoid, statement list) has its own. A `unit role` body starts at the role's name, as in
rakudo. Before, these forms had no ranges at all.

The parser records the name's position (shared with `unit module`) and a `SetLine` for the
statement; the RakuAST conversion fills in the body from the statements the declaration holds
(`src/rakuast/unit_span.rs`). Test: `t/rakuast/rakuast-unit-class-origins.t`.
