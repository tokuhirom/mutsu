# A grammar's cursors are of the grammar's type from the start of the parse

`grammar G { token TOP { <d> { say $/.^name } } ... }` printed `Match` where rakudo prints `G`:
the parse stamped its grammar class on the run's subject only after the engine finished, so every
cursor a code block, an assertion or a `.wrap`ped token's wrapper saw in mid-parse was a plain
`Match`. A parse now opens a `CursorClassScope` on its own subject buffer, and a `MatchTarget`
built for that buffer is born stamped with the grammar's class. A regex match inside the parse on
any other string stays a plain `Match`, and a nested parse of another grammar keeps its own type.
A capture kept in mid-parse is now also the very Match the finished tree holds (#12943).
