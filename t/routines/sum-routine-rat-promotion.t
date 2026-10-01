use Test;

# From the Timer distribution: the routine form of `sum` must promote
# through `+` (Rat, FatRat, BigInt) instead of truncating to Int.
plan 9;

is sum(1, 0.5), 1.5, 'sum(Int, Rat)';
is sum(0.5, 0.25), 0.75, 'sum(Rat, Rat)';
is sum(1/2, 1/3), 5/6, 'sum of two Rats stays exact';
is sum(1.5, 2), 3.5, 'sum(Rat, Int)';
is-approx sum((1..1000) »R/» 1), 7.485470860550344, 'sum of a hyper-reciprocal Seq';
is sum(<1/2 1/2>), 1, 'sum of allomorph list';
is sum(2**70, 1), 2**70 + 1, 'sum with a BigInt';
is sum(1e0, 1/3), 1.3333333333333333e0, 'Num with Rat stays Num';
is sum(1, 2, 3), 6, 'plain Ints still sum';
