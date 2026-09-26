# A measured `|` branch is ranked from its own walk

Inside an LTM declarative-prefix measurement, the plural alternation producer
walked every `|` branch to collect its ends, then ranked the branch with a
second, separate measurement of the same walk. In
`regex A { '{' [ <A> | . ]*? '}' }` that doubled the work at every step of the
loop (#9617): at n = 64 there were 34,370 measurements for 769 real rankings.

The collecting walk now runs in a fate frame of its own, and its prefix is the
furthest of its ends and that frame's fate — exactly what the separate
measurement returned — so only the leading-literal (`litlen`) walk is left to
rank the branch (`src/runtime/regex/regex_ltm_rank_reuse.rs`). The quantifier
DFS's visited-state set also moved from SipHash to FxHash.

On the #9617 repro at n = 64, the parse drops from 452M to about 340M
instructions. The rankings are unchanged. Both mutsu and Rakudo stay quadratic
on this shape, since every ranking runs to the end of the subject.
