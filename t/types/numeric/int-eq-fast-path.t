use Test;

plan 10;

# The Int/Int and Num/Num fast path of `==` (#12151) must agree with the
# generic path on every edge it short-circuits.
my int $a = 5;
my int $b = 5;
my int $c = 6;
ok $a == $b, 'equal native ints';
nok $a == $c, 'unequal native ints';
ok 9223372036854775807 == 9223372036854775807, 'max Int equals itself';
nok 9223372036854775807 == 9223372036854775806, 'adjacent big Ints differ';

my num $x = 0.5e0;
my num $y = 0.5e0;
ok $x == $y, 'equal Nums';
nok NaN == NaN, 'NaN is not equal to itself';
ok 0e0 == -0e0, 'positive and negative zero are equal';

# A user candidate for infix:<==> must still win over the fast path.
{
    class Wrap { has $.v }
    multi sub infix:<==>(Wrap $l, Wrap $r) { $l.v == $r.v }
    ok Wrap.new(v => 1) == Wrap.new(v => 1), 'user == candidate still applies';
}

# Junctions and mixed types take the generic path.
ok (1|2) == 2, 'junction autothreads';
ok 1 == 1.0, 'Int == Rat';
