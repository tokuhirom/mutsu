use Test;

# An argument-less `callsame()` / `nextsame()` takes a direct lane to the
# builtin unless something shadows the name (#10110). Expected values from
# rakudo.

plan 6;

multi sub f(Int $x) { "Int(" ~ callsame() ~ ")" }
multi sub f(Any $x) { "Any" }
is f(1), 'Int(Any)', 'callsame() runs the next candidate';

multi sub g(Int $x) { nextsame(); "unreached" }
multi sub g(Any $x) { "next" }
is g(1), 'next', 'nextsame() hands over and does not return';

multi sub h(Int $x) { callsame }
multi sub h(Any $x) { "bare" }
is h(1), 'bare', 'a bare callsame term too';

multi sub k(Int $x) {
    my &callsame := -> { "lexical" };
    callsame();
}
multi sub k(Any $x) { "builtin" }
is k(1), 'lexical', 'a lexical &callsame shadows the builtin';

{
    my $*modulus = 7;
    multi sub infix:<*>(UInt $a, UInt $b --> UInt) { callsame() mod $*modulus }
    is 5 * 3, 1, 'a user operator reaches the core candidate';
}

class Base { method m { "base" } }
class Derived is Base { method m { "derived+" ~ callsame() } }
is Derived.new.m, 'derived+base', 'a method callsame still works';
