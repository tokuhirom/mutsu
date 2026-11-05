# A quantifier after a bracketed comment at a branch start is dropped, as in rakudo

`|| #|{x}? 'b'` (and the same after `|`, or a group's `[` / `(`) made the
RegexTree parser refuse the regex, so the unit failed to load with "literal
Regex without a declarator tree". rakudo accepts it and the quantifier
(`?`, `*`, `+`, `??`) quantifies nothing: measured against rakudo 2026.09, the
branch just matches `'b'`. With no comment, after a line comment, or outside a
branch it is still `Quantifier quantifies nothing`, and so it stays. ANTLR4::Grammar emits `#|{action}?`
for an ANTLR semantic predicate, so its `PGN.g4` corpus grammar now loads
(`t/03-corpus-compile.t` 46/48 to 47/48; `Java.g4` still fails on a different
construct, the surrogate class tracked in #16592). Closes #16568.
