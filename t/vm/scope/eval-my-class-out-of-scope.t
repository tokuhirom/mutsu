use v6;
use Test;

# A `my class` an EVAL declares is lexical to that EVAL. A later EVAL run from
# the same method of a class with a multi-part name must not reach it, as a
# value or through `::("...")` (Rakuish's REPL runs every cell as an EVAL in
# its session class, #17750). A type the class body itself declares stays
# visible to its methods. Same as raku.

plan 6;

use MONKEY-SEE-NO-EVAL;
class A::B::EvS {
    method ev(Str $code) {
        my $v;
        { CATCH { default { return "fail: " ~ .^name } }; $v = EVAL $code; }
        $v
    }
}
my $s = A::B::EvS.new;
is $s.ev(q{my class Mixed { has $.v }; Mixed.^name}), 'A::B::EvS::Mixed',
    'the EVAL that declares the class sees it';
is $s.ev(q{Mixed.^name}), 'fail: X::Undeclared::Symbols', 'a method call on it from a later EVAL';
is $s.ev(q{my $x = Mixed; $x.^name}), 'fail: X::Undeclared::Symbols', 'an assignment of it';
is $s.ev(q{Mixed}), 'fail: X::Undeclared::Symbols', 'the bare term';
ok $s.ev(q{::("Mixed")}) ~~ Failure, 'an indirect lookup fails';

class Forest {
    my class Frog { method c { 'ribbit' } }
    method f { Frog.c }
}
is Forest.f, 'ribbit', 'a type the class body declares stays visible to its methods';
