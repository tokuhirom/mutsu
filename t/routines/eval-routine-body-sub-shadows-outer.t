use v6;
use Test;

# A `my sub` declared in the body of a routine of an EVAL'd unit shadows the
# unit's own outer sub of the same name when the routine runs, as it does in the
# unit's source text outside an EVAL.

plan 3;

is EVAL(q[sub shadow(Int:D $x) { "outer$x" }
sub caller(Int:D $x) {
    my sub shadow(Int:D $y) { "inner$y" }
    shadow($x)
}
caller(1)]), 'inner1', 'the inner sub wins inside the routine';

is EVAL(q[sub shadow(Int:D $x) { "outer$x" }
sub caller(Int:D $x) {
    my sub shadow(Int:D $y) { "inner$y" }
    shadow($x)
}
caller(1); shadow(2)]), 'outer2', 'and the outer sub is untouched outside it';

throws-like q[sub twice { 1 }; sub twice { 2 }], X::Redeclaration,
    'a redeclaration at the unit top level is still refused';
