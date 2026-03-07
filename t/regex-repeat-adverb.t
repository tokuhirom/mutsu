use Test;

plan 7;

ok('abab' ~~ m:2x/ab/, 'm:2x requires at least two matches');
nok('ab' ~~ m:2x/ab/, 'm:2x fails with fewer matches');

my $exact = 'ababc'.match(rx/ab/, :x(2));
ok($exact, '.match :x(2) succeeds when enough matches exist');
is($exact.join('|'), 'ab|ab', '.match :x(2) returns first two matches');

my $range = 'abacadae'.match(rx/a./, :x(1..3));
is($range.join('|'), 'ab|ac|ad', '.match :x(1..3) applies upper bound');

my $range_floor = 'abacad'.match(rx/a./, :x(2..5));
is($range_floor.join('|'), 'ab|ac|ad', '.match :x(2..5) takes available matches above minimum');

nok('abcabc'.match(rx/a./, :x(3..4)), '.match :x(3..4) fails when minimum is not met');
