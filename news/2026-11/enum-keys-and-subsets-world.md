# An enum or subset declared on a worker is found on the main thread

`await start { EVAL 'subset PosW of Int where * > 0' }; ::('PosW')` was
"No such symbol" on the main thread, and so were the keys of an enum
declared the same way (`::('GreenW')`, `::('ColorW::GreenW')`), though raku
finds all three: a non-`my` enum or subset is a symbol of its package, one
per program.

ADR-17216 slice 3 (part 2, §3.7) makes the table that keeps an `our` enum's
bare keys past the declaring frame (`ModuleToplevel::enum_keys`) a world
table shared by every thread, and teaches the indirect `::('...')` lookup to
answer an `our` subset and an enum's qualified key from the world type
registry instead of only from the frame env. The latter also fixes the
single-thread case: a subset or enum `EVAL`'d inside a routine that has
returned is now found, as in raku. A `my subset` or `my enum` stays lexical.
