use v6;
use Test;
use lib 't/lib';
use ProtoInfixShadowsCore;

# From mod-div-specs: an imported `proto sub infix:<div>` shadows the core
# routine, so its Int candidate is not ambiguous with the core (Int:D, Int:D)
# one, and a call none of its candidates accepts is X::Multi::NoMatch rather
# than the core answer.

plan 4;

my Int $a = 42;
my Int $b = 9;
is 42 div 9, 33, 'the proto family\'s Int candidate takes the call (literals)';
is $a div $b, 33, 'and for Int variables';
throws-like { 42.2 div 9 }, X::Multi::NoMatch, 'a Rat has no candidate: NoMatch, no core fallback';
throws-like { "a" div 1 }, X::Multi::NoMatch, 'a Str has none either';
