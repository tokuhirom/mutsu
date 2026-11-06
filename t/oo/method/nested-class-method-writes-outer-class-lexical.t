use Test;

plan 6;

# A method of a class nested in another class writes the OUTER class body's `my`
# lexical; the outer class's own methods and subs must see that write.
class B {
    my $s = 'init';
    class N { method set { $s = 'n' } }
    method get { $s }
    method run { N.set; $s }
}
is B.run, 'n', 'the outer method reads the nested method\'s write';
is B.get, 'n', 'and so does another method of the outer class';

class H {
    my %states;
    sub des(Str $name) { %states{$name} //= $name.uc; %states.keys.sort.join(',') }
    class W { method reset { %states = (); } }
    method go { %states<a> = 1; W.reset; des('x') }
    method again { W.reset; des('y') }
}
is H.go, 'x', 'a nested `%h = ()` clears the hash the outer sub uses';
is H.again, 'y', 'and does so on every call';

class S {
    state %seen;
    sub note-it(Str $k) { %seen{$k} = 1; %seen.keys.sort.join(',') }
    class R { method clear { %seen = (); } }
    method run { R.clear; note-it('p'); R.clear; note-it('q') }
}
is S.run, 'q', 'a class-body `state` hash behaves the same';

class O {
    my $count = 0;
    class Bump { method go { $count++ } }
    method total { Bump.go; Bump.go; $count }
}
is O.total, 2, 'an increment from a nested class method';
