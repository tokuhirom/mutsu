use v6;
use Test;

# A Rat (or FatRat) whose numerator or denominator is past 2**53 numifies to
# the nearest Num of the exact ratio. Converting each part to a double first
# rounds it on its own: the literal -39.969480000000004 (nude
# -9992370000000001 / 250000000000000) came out as -39.96948.

plan 8;

my $r = -39.969480000000004;
is-deeply $r.nude, (-9992370000000001, 250000000000000), 'the literal keeps its exact parts';
is $r.Num, -39.969480000000004e0, '.Num of a big-numerator Rat';
is $r + 0e0, -39.969480000000004e0, 'mixed Rat + Num arithmetic';
is Num($r), -39.969480000000004e0, 'Num() coercion';
is (9007199254740993/7).Num, 1286742750677284.8e0, 'numerator just past 2**53';
is (1/9007199254740993).Num, 1.1102230246251564e-16, 'denominator past 2**53';
is FatRat.new(-9992370000000001, 250000000000000).Num, -39.969480000000004e0, 'FatRat too';
ok $r < -39.96948e0, 'comparison with a Num uses the exact value';
