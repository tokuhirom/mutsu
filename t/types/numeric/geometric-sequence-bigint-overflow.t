use Test;

# A growing integer geometric sequence (`1, 2, 4 ... *`) must not overflow i64
# and panic — Raku's Int is arbitrary precision, so values past 2**62 promote to
# BigInt and stay exact. Regression: a "multiply with overflow" panic in the
# lazy sequence generator (integration/advent2010-day04.t).

plan 6;

my @p = 1, 2, 4 ... *;
is @p[^10].join(" "), "1 2 4 8 16 32 64 128 256 512", "small powers of two exact";
is @p[63], 9223372036854775808, "2**63 (just past i64::MAX) is exact";
is @p[69], 590295810358705651712, "2**69 stays exact BigInt";
ok @p[100] > @p[99], "deep geometric index does not panic and grows";

# ratio 3 geometric also stays exact
my @t = 1, 3, 9 ... *;
is @t[40], 3**40, "powers of three exact past i64";

# arithmetic int sequence unaffected
my @a = 2, 4 ... *;
is @a[10], 22, "arithmetic sequence still correct";
