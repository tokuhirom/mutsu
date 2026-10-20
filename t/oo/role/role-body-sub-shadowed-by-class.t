use Test;

# A role method calls the role body's own `sub`, and the composing class's
# methods call the class's, when both declare a `sub` of the same name
# (#14177). Each method resolves a bare call in the scope it was written in.

plan 7;

role R {
    my $count = 10;
    method m { f() }
    method via-g { g() }
    sub f { ++$count }
    sub g { f() + 100 }
}
class K does R {
    sub f { "class f" }
    method k { f() }
}

is K.new.k, 'class f', "the class's method calls the class's sub";
is K.new.m, 11, "a role method calls the role body's sub";
is K.new.m, 12, "the role body's sub keeps its per-composer state";
is K.new.via-g, 113, "a role body sub calls a shadowed sibling sub of the role";
is K.new.k, 'class f', "the class's sub is still the class's afterwards";

class L does R { }
is L.new.m, 11, 'another composer has its own role sub state';

role Q { method q { h() }; sub h { 'role h' } }
class C does Q { sub h { 'class h' }; method c { h() } }
is C.new.q ~ '/' ~ C.new.c, 'role h/class h', 'a role without body lexicals';
