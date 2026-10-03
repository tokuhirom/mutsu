use Test;

# A range with a BigInt endpoint counts from its endpoints; it is not capped
# at the list-expansion limit (#11402).
plan 8;

my $m = 18446744073709551615;
is (^$m).elems, 18446744073709551615, '(^$m).elems is exact';
is (^$m).Int, 18446744073709551615, '(^$m).Int is exact';
is (^$m).Numeric, 18446744073709551615, '(^$m).Numeric is exact';
is (0..$m).elems, 18446744073709551616, '(0..$m).elems is exact';
is (0^..^$m).elems, 18446744073709551614, '(0^..^$m).elems is exact';
is (1..^1).elems, 0, 'an empty range has no elements';
is (2**64 .. 2**64 + 4).elems, 5, 'both endpoints BigInt';
is (^5).elems, 5, 'a small range is unchanged';
