use Test;

# A named capture group `$<x>=(...)` aliases the group to the name and does NOT
# consume a positional capture number (Raku semantics): in `/$<x>=(\w)(\d)/`,
# `$<x>` is the \w and `$0` is the \d. A named NON-capturing group `$<x>=[...]`
# leaves its inner captures' positional numbers intact.

plan 12;

ok "a5" ~~ /$<x>=(\w)(\d)/, 'match with leading named capture';
is ~$<x>, 'a', 'named capture $<x> is the \w';
is ~$0,   '5', 'positional $0 is the \d (named did not consume slot 0)';

ok "a5b" ~~ /$<x>=(\w)(\d)(\w)/, 'match with named + two positionals';
is ~$<x>, 'a', '$<x> = a';
is ~$0,   '5', '$0 = 5';
is ~$1,   'b', '$1 = b';

# named capture in the middle
ok "ab" ~~ /(\w)$<y>=(\w)/, 'match with named capture after a positional';
is ~$0,   'a', 'positional before named is $0';
is ~$<y>, 'b', 'named capture after positional';

# named NON-capturing group keeps inner positional numbers
ok "a5" ~~ /$<z>=[(\w)(\d)]/, 'named non-capturing group';
is "{~$<z>}|{~$0}|{~$1}", 'a5|a|5', 'inner captures of [...] keep $0/$1';
