use Test;

# Junctive `&` binds tighter than `|` and `^` (`1 | 2 & 3` is
# `any(1, all(2, 3))`) -- found with Data::Dump::Tree's `1 | 'a' & True` (#15627).

plan 8;

is (1 | 2 & 3).raku, 'any(1, all(2, 3))', '| then &';
is (1 & 2 | 3).raku, 'any(all(1, 2), 3)', '& then |';
is (1 | 2 & 3 | 4).raku, 'any(1, all(2, 3), 4)', '& between two |';
is (1 ^ 2 & 3).raku, 'one(1, all(2, 3))', '^ with &';
is (1 & 2 & 3).raku, 'all(1, 2, 3)', '& chain stays flat';
is (1 | 2 | 3).raku, 'any(1, 2, 3)', '| chain stays flat';
is ((1 | 2) & 3).raku, 'all(any(1, 2), 3)', 'parentheses still group';
is (1 | 2 ?| 3).raku, 'any(1, Bool::True)', '?| keeps binding tighter than |';
