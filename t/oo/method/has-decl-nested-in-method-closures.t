use v6;
use Test;

# `has` is a compile-time declarator: rakudo installs the attribute wherever it
# lexically sits in the class body (#8441, see has-decl-nested-in-method.t) —
# also in a closure, a phaser or a `do` block inside a method, and in source
# order.

plan 6;

class C {
    method m { if 1 { has $.a = 1 }; has $.b = 2 }
}
is C.^attributes.map(*.name).join(' '), '$!a $!b', 'nested attributes keep source order';

class D {
    method m { my $f = { has $.z = 3 }; }
}
is D.^attributes.map(*.name).join(' '), '$!z', 'a has in a closure inside a method is installed';
is D.new.z, 3, 'and gets its default';

class E {
    method m { LEAVE { has $.q = 4 } }
}
is E.^attributes.map(*.name).join(' '), '$!q', 'a has in a phaser inside a method is installed';

class F {
    method m { my $x = do { has $.d = 5; 1 } }
}
is F.new.d, 5, 'a has in a do block inside a method is installed';

class G {
    method m { class Inner { has $.i } }
}
is G.^attributes.elems, 0, 'a nested class keeps its own attributes';
