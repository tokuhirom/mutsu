use v6;
use Test;

# `.slice` selects elements at the given (increasing) index arguments. A Range
# argument (`3..6`) expands to every index it spans, so `.slice(0, 3..6, 8)`
# gathers indices 0,3,4,5,6,8. Previously Range arguments were silently
# ignored. The result is a Seq. (The gists below were verified against `raku`.)

plan 7;

is (1..10).slice(0, 3..6, 8).raku, '(1, 4, 5, 6, 7, 9).Seq', 'mixed int and range indices';
is (1..10).slice(2, 4).raku,       '(3, 5).Seq',             'plain int indices';
is <a b c d e>.slice(1, 3..4).raku, '("b", "d", "e").Seq',   'range on a word list';
is (10, 20, 30, 40).slice(0..2).raku, '(10, 20, 30).Seq',    'a single range argument';
is ('x'..'z').slice(0, 2).raku,    '("x", "z").Seq',         'slice over a Range receiver';
is (1..10).slice().raku,           '().Seq',                 'no arguments is empty';

# Range indices past the end of the list are skipped.
is (1..3).slice(0, 2..5).raku,     '(1, 3).Seq',             'range indices past the end are skipped';
