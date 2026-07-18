use v6;
use Test;

plan 5;

# A literal `Nil` parameter must out-narrow other candidates for a Nil
# argument regardless of declaration order (URI's `authority(Nil)`).
class A {
    multi method m(Str() $s) { "str:$s" }
    multi method m(Nil) { "nil" }
    multi method m() { "none" }
}

is A.new.m(Nil), 'nil', 'Nil argument picks the (Nil) candidate';
is A.new.m('x'), 'str:x', 'Str argument picks the coercion candidate';
is A.new.m(), 'none', 'no argument picks the nullary candidate';

# Order-independence: Nil candidate declared last still wins.
class B {
    multi method m($x) { "any" }
    multi method m(Nil) { "nil" }
}
is B.new.m(Nil), 'nil', '(Nil) wins over ($x) for a Nil argument';
is B.new.m(42), 'any', 'non-Nil argument still reaches ($x)';
