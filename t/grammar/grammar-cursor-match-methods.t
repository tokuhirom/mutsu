use Test;

# A grammar cursor's `Match` methods (`.ast`, `.from`, `.Str`, ...) answer
# from the `Match` method table unless the grammar declares the method
# itself. Each call is repeated so a memoized verdict is exercised too.

plan 14;

grammar G {
    token TOP { <a> <b> }
    token a { 'a' }
    token b { 'b' }
}
class A { method TOP($/) { make 42 }; method a($/) { make 'x' } }

my $m = G.parse('ab', :actions(A));
for ^2 {
    is $m.ast, 42, '.ast of a cursor';
    is $m<a>.made, 'x', '.made of a child cursor';
    is ($m.from, $m.to), (0, 2), '.from/.to of a cursor';
    is $m<b>.Str, 'b', '.Str of a child cursor';
}

grammar H {
    token TOP { 'x' }
    method ast { 'own' }
}
my $h = H.parse('x');
is $h.ast, 'own', 'a grammar method shadows Match.ast';
is $h.ast, 'own', 'and keeps shadowing it on a repeated call';

grammar Base { token TOP { 'y' }; method Str { 'base-str' } }
grammar Derived is Base { }
my $d = Derived.parse('y');
is $d.Str, 'base-str', 'an inherited grammar method shadows Match.Str';
is $d.from, 0, 'an unshadowed method still answers';

my $p = 'ab' ~~ /a/;
is $p.Str, 'a', 'a plain match is unaffected';
is G.parse('ab').^name, 'G', 'a cursor still reports its grammar';
