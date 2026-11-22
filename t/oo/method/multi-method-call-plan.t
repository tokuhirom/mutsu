use Test;

# A multi method whose winner is a function of the argument types (no `where`,
# literal, subset or smiley candidate) is replayed from a per-site method-call
# plan after its first call with those types (ADR-15737 D1b). Deferral and
# `samewith` from the planned winner, and a family with a value-dependent
# candidate (never planned), must give rakudo's answers on every call.
plan 6;

class Shape {
    multi method scale(Int $n) { $n * 2 }
    multi method scale(Rat $r) { ($r * 4).Int }
    multi method scale(Str $s) { $s.chars }
}
my $s = Shape.new;
is (^6).map({ $s.scale((3, 3/4, "abcd")[$_ % 3]) }).join(','), '6,3,4,6,3,4',
    'each argument type keeps its own winner';

class Base {
    multi method m(Int $x)  { "base-int $x" }
    multi method m(Cool $x) { "base-cool $x" }
}
class Kid is Base {
    multi method m(Int $x) { "kid " ~ callsame() }
    multi method m(Str $x) { "kid-str " ~ nextsame() }
}
my $k = Kid.new;
is (^4).map({ $k.m((1, "x")[$_ % 2]) }).join('|'),
    'kid base-int 1|base-cool x|kid base-int 1|base-cool x',
    'callsame and nextsame out of a planned winner';

class Sw {
    multi method f(Int $x) { $x > 0 ?? samewith($x - 1) + 1 !! 0 }
    multi method f(Str $x) { samewith($x.chars) }
}
is (^3).map({ Sw.new.f(("abc", 4)[$_ % 2]) }).join(','), '3,4,3',
    'samewith out of a planned winner';

class Wh {
    multi method w(Int $x where * > 5) { "big" }
    multi method w(Int $x)             { "small" }
}
my $w = Wh.new;
is (^4).map({ $w.w((3, 9)[$_ % 2]) }).join(','), 'small,big,small,big',
    'a family with a where candidate decides per value';

class Def {
    multi method d(Int:D $x) { "defined" }
    multi method d(Int:U $x) { "type object" }
}
my $d = Def.new;
is (^4).map({ $d.d((1, Int)[$_ % 2]) }).join(','), 'defined,type object,defined,type object',
    'a smiley family decides per definedness';

# A Proxy argument's key does not change with what it FETCHes, so a planned
# winner for a plain value must not answer for it.
class Px { multi method m(Numeric $v) { "num $v" }; multi method m(Str $v) { "str" } }
my $px = Px.new;
my $store = 'abc';
my $proxy := Proxy.new(FETCH => sub ($) { $store }, STORE => sub ($, $v) { });
is ($px.m(5), $px.m(6), $px.m($proxy)).join(' '), 'num 5 num 6 str',
    'a Proxy argument after planned plain calls';
