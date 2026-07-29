use Test;

plan 3;

# Default .sort must order big rationals numerically — without the big-rat
# branch in compare_values they fell to the string fallback and mis-ordered
# (DBIish 24-mysql-types 'Large Rats' compares a sorted list against
# SQL ORDER BY).

my @v = 10.43, -10.34,
        "0.123456789012345678901".FatRat, "-0.123456789012345678901".FatRat,
        (2 ** 63 + 0.1).FatRat, (-2 ** 63 + 0.1).FatRat,
        (2 ** 80 + 0.1).FatRat, (-2 ** 80 + 0.1).FatRat;

is-deeply @v.sort.map(*.Str).list,
    ("-1208925819614629174706175.9", "-9223372036854775807.9", "-10.34",
     "-0.123456789012345678901", "0.123456789012345678901", "10.43",
     "9223372036854775808.1", "1208925819614629174706176.1"),
    'mixed big FatRats sort numerically';

cmp-ok (-2**80 + 0.1).FatRat, '<', -0.5, 'big negative FatRat compares less than small';
cmp-ok (2**80).Rat, '>', 10.43, 'big Rat compares greater than small';
