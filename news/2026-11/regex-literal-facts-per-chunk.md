# A regex literal no longer rescans its pattern on every evaluation

Evaluating a regex literal (`LoadRegexClosure`) asked two questions of its
pattern text every time: does it interpolate a string as regex source
(`<{ }>`), and does it embed code that runs at match time (`$( )`, `<{ }>`,
or a `{ }` block that is not a quantifier range). That took three substring
scans of the pattern, even for a literal that captures nothing, whose
opcode cost was documented as O(1). Both answers are pure functions of the
constant pattern, so they are now settled once for every regex literal of a
compiled chunk (`CompiledCode::regex_literal_facts`).

A literal smartmatch (`$l ~~ / 'code=' \d+ /`, `bench-regex-match` line 5
shape), `MUTSU_JIT=off`, callgrind slope: **15.69k -> 14.28k instructions per
match (-9%)**. Every regex literal evaluated in a loop gets the same saving
(#19296).
