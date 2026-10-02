# `use trace` numbers the statements of a regex code block

Rakudo's statement number (`$*STATEMENT_ID`, the leading number of a
`use trace` header) also counts the statements of a regex code block. A block
takes one number per statement, plus one for the failed attempt at its
closing brace, and blocks nested inside take their own numbers. mutsu parses
such a block from a copy of its text, so these attempts landed outside the
unit's source and were dropped. Every statement after a regex with a code
block was numbered too low.

The block's fragment parse now borrows the enclosing unit's attempt list and
records each position shifted by the offset the copy came from
(`src/parser/primary/fragment_attempts.rs`). This covers `/.../`, `m//`,
`rx//`, `<?{ }>`, `<{ }>` and `token` declarations (#10669). Tracing the
block's own statements, and the adverb forms that are parsed only at match
time, are #10818.
