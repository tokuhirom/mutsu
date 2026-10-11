use Test;

# `is default` on a multi method breaks only a tie on the full narrowness
# order, as in sub dispatch: a narrower `where` candidate wins first (#12775).

plan 6;

class C {
    multi method m(Int $x where * > 0) { "where" }
    multi method m(Int $x) is default  { "default" }
}
is C.m(5), 'where', 'a narrower where candidate beats an is default one';
is C.m(-1), 'default', 'the is default candidate takes what the where rejects';

class D {
    multi method m(Int $x) { "a" }
    multi method m(Int $x) is default { "b" }
}
is D.m(1), 'b', 'is default breaks an exact tie (declared second)';

class E {
    multi method m(Int $x) is default { "a" }
    multi method m(Int $x) { "b" }
}
is E.m(1), 'a', 'is default breaks an exact tie (declared first)';

class F {
    multi method m(Int $x) { "int" }
    multi method m(Cool $x) is default { "cool" }
}
is F.m(1), 'int', 'a nominally narrower candidate beats an is default one';

class P { multi method m(Int $x) { "P" } }
class Q is P { multi method m(Int $x) is default { "Q" } }
is Q.m(1), 'Q', 'is default across an inherited tie';
