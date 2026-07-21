use v6;
use Test;

# Left-exclusive sequence operators (`^...` / `^...^`) must parse as an
# unparenthesized listop argument, not only inside parentheses.
# Regression: `say 1 ^... 4` failed to parse while `say (1 ^... 4)` worked.

plan 10;

is (1 ^... 4).join(","),  "2,3,4",   "^... in parens";
is (1 ^...^ 4).join(","), "2,3",     "^...^ in parens";

# As a bare listop argument (the path that used to fail to parse).
is-deeply (1 ... 4),   (1, 2, 3, 4),  "... listop arg";
is-deeply (1 ...^ 4),  (1, 2, 3),     "...^ listop arg";
is-deeply (1 ^... 4).List,  (2, 3, 4),  "^... listop arg";
is-deeply (1 ^...^ 4).List, (2, 3),     "^...^ listop arg";

# Multi-seed geometric sequences with left-exclusive endpoints.
is-deeply (1, 2, 4 ^... 32).List,  (2, 4, 8, 16, 32),  "multi-seed ^...";
is-deeply (1, 2, 4 ^...^ 32).List, (2, 4, 8, 16),       "multi-seed ^...^";

# Descending and string ranges are unaffected.
is-deeply (4 ... 1), (4, 3, 2, 1), "descending ...";
is-deeply ('a' ^... 'd').List, ('b', 'c', 'd'), "string ^...";
