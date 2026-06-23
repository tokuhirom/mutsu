use Test;

plan 13;

# A BigInt is an Int; its .raku/.perl must render a plain integer that
# round-trips as an Int, not a float with a spurious ".0".
is (10 ** 20).raku, '100000000000000000000', 'big power .raku has no .0';
is (10 ** 20).perl, '100000000000000000000', 'big power .perl has no .0';
is (2 ** 70).raku, '1180591620717411303424', 'big power of two .raku';
is 100000000000000000000.raku, '100000000000000000000', 'big literal .raku';
is (99999999999999999999 + 1).raku, '100000000000000000000', 'big sum .raku';
is (-(10 ** 20)).raku, '-100000000000000000000', 'negative big int .raku';
is [10 ** 20, 2 ** 70].raku, '[100000000000000000000, 1180591620717411303424]',
    'big ints inside an array .raku';
is (a => 10 ** 20).raku, ':a(100000000000000000000)', 'big int as pair value .raku';

# .raku output must EVAL back to the same Int.
is (10 ** 20).raku.EVAL, 10 ** 20, 'big int .raku round-trips via EVAL';

# `/` on integers always yields a Rat, even for big values that divide evenly:
# `555…/5` is a Rat (111…/1), not an Int — so .WHAT is Rat and .raku keeps .0.
my $big = 555555555555555555555555555555555555555555555 / 5;
is $big.^name, 'Rat', 'big exact integer division yields a Rat, not an Int';
is $big.Str, '111111111111111111111111111111111111111111111',
    'big exact-division Rat .Str has no .0';
is $big.raku, '111111111111111111111111111111111111111111111.0',
    'big exact-division Rat .raku keeps .0 (it is a Rat)';
is (10 / 2).WHAT.raku, 'Rat', 'small exact integer division still yields a Rat';
