use v6;
use Test;

# A reduction metaop `[op]` must accept the Unicode operator aliases that mutsu
# already handles as plain infixes (× ÷ − ≤ ≥ ≠ ∪ ∩). Previously `[×]` (and the
# others) failed to parse ("expected expression after multiplicative operator")
# because they were missing from the reduction-operator whitelist, and even once
# parsed the fold reached a non-existent `infix:<×>` lookup. Surfaced by
# Prime::Factor (`[×] .list for these «**« upto`).

plan 9;

is ([×] 6, 2, 3), 36,   '[×] reduces as multiplication';
is ([÷] 24, 2, 3), 4,   '[÷] reduces as division';
is ([−] 10, 2, 3), 5,   '[−] (U+2212) reduces as subtraction';
is ([≤] 1, 2, 3), True, '[≤] reduces as chained <=';
is ([≥] 3, 2, 1), True, '[≥] reduces as chained >=';
is ([≠] 1, 2), True,    '[≠] reduces as !=';
is-deeply ([∪] (1,2).Set, (2,3).Set), (1,2,3).Set, '[∪] reduces as set union';
is-deeply ([∩] (1,2,3).Set, (2,3,4).Set), (2,3).Set, '[∩] reduces as set intersection';

# The scan (triangular) form works too.
is-deeply ([\×] 2, 3, 4).List, (2, 6, 24), '[\×] scan reduction';
