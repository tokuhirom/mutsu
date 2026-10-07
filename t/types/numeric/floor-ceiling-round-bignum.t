use Test;

# Found via Net::Ethereum t/14.t: the routine forms floor/ceiling/round
# returned 0 for a big Rat / big Int, while the method forms were right.
plan 9;

my $r = <449306674870008090516/100000000>;
is floor($r), 4493066748700, 'floor of a big Rat';
is ceiling($r), 4493066748701, 'ceiling of a big Rat';
is round($r), 4493066748700, 'round of a big Rat';
is floor(-$r), -4493066748701, 'floor of a negative big Rat';
is floor(4493066748700.08090516), 4493066748700, 'floor of a big Rat literal';
is floor(2**70), 2**70, 'floor of a big Int';
is ceiling(2**70), 2**70, 'ceiling of a big Int';
is round(2**70), 2**70, 'round of a big Int';

my $a = 9000000000000;
is floor(($a * 5000000) / (3338477 * 3)), 4493066748700, 'quotient of a >i64 product';
