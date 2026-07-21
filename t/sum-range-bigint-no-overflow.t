use v6;
use Test;

# `sum RANGE` with i64 endpoints must not overflow: the Gauss product is
# computed in wider precision and promoted to a BigInt past i64 range.
# (raku-doc Language/structures.rakudoc — this used to panic in mutsu.)
is sum(1 .. 9_999_999_999_999), 49999999999995000000000000,
    'sum of a huge inclusive range returns the exact BigInt';

is sum(1 .. 1_000_000_000), 500000000500000000,
    'sum of a large i64-fitting range is exact';

# Exclusive endpoints stay correct at large scale.
is sum(1 ..^ 9_999_999_999_999), 49999999999985000000000001,
    'exclusive-end huge range is exact';
is sum(1 ^.. 9_999_999_999_999), 49999999999994999999999999,
    'exclusive-start huge range is exact';

# Small ranges are unchanged (all four boundary flavours).
is sum(1 .. 100),   5050, 'inclusive small range';
is sum(1 ..^ 100),  4950, 'exclusive-end small range';
is sum(1 ^.. 100),  5049, 'exclusive-start small range';
is sum(1 ^..^ 100), 4949, 'exclusive-both small range';

# Empty / degenerate ranges sum to 0.
is sum(5 .. 3), 0, 'reversed (empty) range sums to 0';
is sum(0 .. 0), 0, 'single-zero range sums to 0';

# The result is an Int (BigInt) type, arithmetic keeps working on it.
ok sum(1 .. 9_999_999_999_999) ~~ Int, 'huge range sum is an Int';
is sum(1 .. 9_999_999_999_999) + 5, 49999999999995000000000005, 'BigInt sum keeps arithmetic';

done-testing;
