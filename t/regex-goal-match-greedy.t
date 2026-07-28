use Test;

plan 6;

# `A ~ B C` matches A, then C, then B. It does NOT stop C at the first position
# where B could match — the inner term keeps its own (greedy) priority order.
# YAMLish's `single-quoted` is `"'" ~ "'" [ <single-bare> | "''" ]*`, so
# `'ab''cd'` must match in full rather than stopping at the doubled quote.
my $doubled = "'ab''cd'";

is ~($doubled ~~ / ^ "'" ~ "'" [ <-['\ ]>+ | "''" ]* /), $doubled,
    'a goalpost takes the greedy inner match';
is ~($doubled ~~ / ^ "'" ~ "'" [ "''" | <-['\ ]>+ ]* /), $doubled,
    'alternative order does not matter';
is ~($doubled ~~ / ^ "'" ~ "'" [ <-['\ ]> | "''" ]* /), $doubled,
    'nor does a single-char alternative';
is ~($doubled ~~ / ^ "'" ~ "'" ( <-['\ ]>+ | "''" )* /), $doubled,
    'a capturing inner group behaves the same';
is ~($doubled ~~ / ^ "'" [ <-['\ ]>+ | "''" ]* "'" /), $doubled,
    'and matches what the goalpost-free spelling does';

# A shorter inner match is still available on backtracking when the rest of the
# pattern demands it.
is ~("'ab'cd" ~~ / ^ "'" ~ "'" [ <-['\ ]>+ ]* 'cd' /), "'ab'cd",
    'the goalpost backtracks when what follows requires it';
