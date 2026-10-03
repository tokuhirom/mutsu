use Test;

# From Data::MessagePack (t/202-unpack-int): `$n -^ 0xffffffff - 1` is
# `$n - ^0xffffffff - 1`, so a Range operand numifies to its element count,
# which must be exact beyond the 1_000_000 list-expansion cap.

plan 9;

is 100 - ^1000001, -999901, 'excl Int range count past the expansion cap';
is 100 - (1..2000000), -1999900, 'incl Int range count past the expansion cap';
is 100 - (0..^3000000), -2999900, '0..^N count';
is 0xffff7fff -^ 0xffffffff - 1, -32769, 'msgpack int32 decode';
is 0xffffffff7fffffff -^ 0xffffffffffffffff - 1, -2147483649, 'msgpack int64 decode (BigInt endpoint)';
is 100 - (5..1), 100, 'empty range numifies to 0';
is 100 - (1^..^1), 100, 'empty range with both ends excluded numifies to 0';
is 10 - (1^..5), 6, 'RangeExclStart count';
is 7 - ^3, 4, 'small range still works';
