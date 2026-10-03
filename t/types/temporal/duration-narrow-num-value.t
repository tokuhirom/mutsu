use Test;

# A Duration built by Instant arithmetic holds a Num inside (#11273). Its
# `.narrow` converted that Num to a Rat with a hand-rolled decimal-digit loop
# that overflowed i64 for a tiny value such as `now - now` (a debug-build
# panic, garbage in release). It now narrows through the same Num -> Rat
# conversion `Duration.new` / `.Rat` use, like Rakudo, whose Duration always
# holds a Rat (#11271).

plan 5;

my $d = now - now;
lives-ok { $d.narrow }, '(now - now).narrow does not overflow';
ok $d.narrow ~~ Real, '... and gives a Real';
ok abs($d.narrow - $d) < 1e-5, '... close to the Duration itself';

is Duration.new(2).narrow.^name, 'Int', 'integral Duration narrows to Int';
is Duration.new(1.5).narrow, 1.5, 'fractional Duration narrows to its Rat';

# vim: expandtab shiftwidth=4
