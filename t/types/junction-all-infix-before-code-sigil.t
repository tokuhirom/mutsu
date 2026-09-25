use Test;

# After a complete term (`self`, a variable, a method call, a literal), a
# spaced `&name` is in infix position: it is the all-junction `&`, not a
# `&name` code-variable term (GLFW writes `set-...-callback(self &callback)`).
# After a listop name, `&name` is still an argument.

plan 7;

sub h { 1 }
my $x = 1;
my &c = { 2 };

isa-ok ($x &c), Junction, '$x &c is an all-junction';
isa-ok (1 &h), Junction, 'literal before &name';
my @a = 1, 2;
isa-ok (@a.elems &h), Junction, 'method call before &name';

class A { method m(&cb) { self &cb } }
isa-ok A.new.m({ 1 }), Junction, 'self &cb is an all-junction';

sub f(&g) { g() }
is (f &h), 1, 'a listop still takes &name as its argument';
is f(&h), 1, 'a parenthesized call is unaffected';
isa-ok ($x&c), Junction, 'glued form still an all-junction';
