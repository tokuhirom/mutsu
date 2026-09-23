use v6;
use Test;

# A pointy block's `[@items, $value]` signature must bind @items to the
# nested array itself, not to an extra one-element list around it.
plan 4;

my @cases = ([3, 4], 0), ([0, 4, 8, 12, 16], 1);
for @cases -> [ @index, $expected ] {
    is-deeply @index, @cases[$expected ?? 1 !! 0][0],
        'the array parameter receives the nested array elements';
    is $expected, +@index %% 5 ?? 1 !! 0,
        'the scalar parameter receives the sibling value';
}

done-testing;
