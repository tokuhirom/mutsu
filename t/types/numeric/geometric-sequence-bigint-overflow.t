use Test;

# A growing integer geometric sequence (`1, 2, 4 ... *`) must not overflow i64
# and panic — Raku's Int is arbitrary precision, so values past 2**62 promote to
# BigInt and stay exact. Regression: a "multiply with overflow" panic in the
# lazy sequence generator (integration/advent2010-day04.t).

plan 12;

my @p = 1, 2, 4 ... *;
is @p[^10].join(" "), "1 2 4 8 16 32 64 128 256 512", "small powers of two exact";
is @p[63], 9223372036854775808, "2**63 (just past i64::MAX) is exact";
is @p[69], 590295810358705651712, "2**69 stays exact BigInt";
ok @p[100] > @p[99], "deep geometric index does not panic and grows";

# ratio 3 geometric also stays exact
my @t = 1, 3, 9 ... *;
is @t[40], 3**40, "powers of three exact past i64";
is @t[50].raku, '717897987691852588770249',
    'powers of three retain Int precision after promotion';

my @eleven = 1, 11, 121 ... *;
is @eleven[19].raku, '61159090448414546291',
    'an integer ratio other than two stays exact past i64';
is @eleven[19].WHAT, Int, 'the promoted geometric element remains an Int';
is (1, 11, 121 ... 10**30).tail.raku, '144209936106499234037676064081',
    'a BigInt endpoint stops the exact geometric sequence before it is crossed';

my @rational = 2, 3, 9/2 ... *;
is @rational[40].raku, '22114664.641880024284546379931271076202392578125',
    'a rational ratio stays exact in the deferred generator';
is @rational[40].WHAT, Rat, 'the deferred rational ratio retains Rat type';

# arithmetic int sequence unaffected
my @a = 2, 4 ... *;
is @a[10], 22, "arithmetic sequence still correct";
