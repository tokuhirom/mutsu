use Test;

plan 2;

grammar G {
    token TOP { <a> }
    token a { <b> }
    token b { 'x' }
}
class A {
    method b($/) { make 'B' }
    method a($/) { make $<b>.made ~ 'A' }
}

my $match = G.parse('x', :actions(A));
is $match<a><b>.made, 'B', 'child action result survives parent rebuild';
is $match<a>.made, 'BA', 'parent action reads the rebuilt child';
