use Test;

# From the CRDT ecosystem distribution's test helper: `$_ == $turn` where
# `$turn` is a Range (`3..7`). `Range.Numeric` is `.elems`, so a lone Range
# meeting a number compares by its element count.

plan 12;

my $r = 3..7;
ok 5 == $r, 'Int == Range compares with the element count';
ok $r == 5, 'Range == Int';
nok 4 == $r, 'a different count is not equal';
ok 4 != $r, '!=';
is 5 <=> $r, Order::Same, '<=> uses the element count';
is 6 <=> $r, Order::More, '<=> more';
ok 5 <= $r, '<= still works';
nok 5 < $r, '< is on the element count';
ok (3..^7) == 4, 'an excluding range counts its elements';
ok (1..3) == (4..6), 'two ranges with the same size are equal';
is (try { "abc" == 1 }) // 'failure', 'failure', 'a non-numeric string is still a Failure';
is (2..4) <=> 3, Order::Same, 'Range <=> Int';
