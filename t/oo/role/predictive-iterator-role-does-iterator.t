use v6;
use Test;

# From Sequence::Generator: `role PredictiveIterator does Iterator`, so a class
# composing it is also an Iterator (return-type checks, ~~, .^roles).

plan 5;

class P does PredictiveIterator {
    method pull-one() { IterationEnd }
    method count-only() { 0 }
    method bool-only() { False }
}

ok P.new ~~ Iterator, 'a PredictiveIterator class is an Iterator';
ok P.new ~~ PredictiveIterator, 'and a PredictiveIterator';
is P.^roles.map(*.^name).List, <PredictiveIterator Iterator>, '.^roles lists both';
sub f(--> Iterator:D) { P.new }
isa-ok f(), P, 'an Iterator:D return type accepts it';
is PredictiveIterator.^roles.map(*.^name).List, <Iterator>, 'the role itself composes Iterator';
