use Test;

# `log($base)`, `exp($base)` and `atan2($x)` read their argument as a number
# (Rakudo's `Num(Real) $base`): an instance of a class composing `Real` is
# coerced through its `Bridge` before the built-in method runs (ADR-11276
# §9.184).

plan 6;

class R does Real {
    has $.n;
    method Bridge { $!n.Num }
}

is 100.log(R.new(n => 10)), 2, 'Int.log with a Real object base';
is 2.exp(R.new(n => 3)), 9, 'Int.exp with a Real object base';
is-approx 1.atan2(R.new(n => 1)), 0.7853981633974483, 'Int.atan2 with a Real object';
is-approx 8e0.log(R.new(n => 2)), 3, 'Num.log';
is-approx (1+2i).log(R.new(n => 2)), 1.1609640474436813+1.5972779646881088i, 'Complex.log';

grammar G { token TOP { \d+ } }
class A { method exp($/) { 'own' } }
is A.new.exp(G.parse('12')), 'own', "a class's own exp gets its argument unchanged";
