# A `where` on a multi method's invocant picks the candidate

`multi method m($self where { ... }:)` (and `\SELF where {...}:`, with or without a type) had its invocant constraint's type checked during candidate matching but its `where` clause never evaluated, so the first candidate of such a pair always won. The clause is now run against the invocant, as for any other parameter.

Red 0.2.7's `ColumnMethods` declares `multi method elems(\SELF where { .returns ~~ Json }:)` beside a candidate for every other column type; the Json one was chosen for every column, so `rows.map(*.col3.elems)` built `JSON_EACH` over a plain string column and SQLite failed with `malformed JSON` (`t/88-projection-chains`).
