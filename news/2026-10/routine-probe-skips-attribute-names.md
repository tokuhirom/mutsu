# By-name `&!attr` reads no longer scan every routine

A closure stored in an attribute and called or passed by name (`&!pGExpr` in
FunctionalParsers' EBNF parser) first looks the attribute up, and when that
misses it asks whether a routine of that name exists. Part of that question
walked every registered routine on each read: the multi-candidate probe
could not consult the base-name index from the code path it ran on.

That probe now reads the index whenever an entry is already there. A name with
an attribute twigil (`!x`, `.x`), which no routine can have, now gets its
answer immediately. On the FunctionalParsers EBNF probe the whole run executes
4% fewer instructions. This is part of ADR-12529 phase 2.
