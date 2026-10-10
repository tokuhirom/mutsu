use Test;
# From Version::Repology t/02-equal-less: big Ints beyond 2**53 must compare
# exactly under <=> and cmp, not through an f64 approximation.
plan 7;

my $a = 99999999999999999999999999999999999998;
my $b = 99999999999999999999999999999999999999;
is ($a <=> $b), Less, 'BigInt <=> BigInt (less)';
is ($b <=> $a), More, 'BigInt <=> BigInt (more)';
is ($a cmp $b), Less, 'BigInt cmp BigInt';
is (2**70 <=> 2**70 + 1), Less, 'neighbouring 2**70 values differ';
is (2**70 <=> 2**70), Same, 'equal BigInts are Same';
is (2**64 <=> 5), More, 'BigInt <=> Int';
is (5 <=> 2**64), Less, 'Int <=> BigInt';
