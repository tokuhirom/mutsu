use v6;
use Test;

# An `is rw` multi candidate matches only a writable-lvalue argument — a
# call-site property, not an arg-type one. The type-keyed multi resolution
# cache must therefore never cache such a multi: `m($var)` and `m("lit")`
# carry the same type key but need different winners.
# (found via Text::IO::String's `multi method new (Str $str! is rw)` /
#  `(Str $str!)` pair in Text::CSV's 85_util.t)

plan 8;

class C {
    multi method m (Str $s! is rw) { "rw:" ~ $s }
    multi method m (Str $s!)       { "ro:" ~ $s }
}

my Str $d = "x";
is C.m($d),    "rw:x",   'writable variable arg picks the rw candidate';
is C.m("lit"), "ro:lit", 'literal arg falls to the non-rw candidate (not the cached rw winner)';
is C.m($d),    "rw:x",   'variable arg picks rw again after the literal call';

# Reverse order: the literal must not freeze the ro candidate for variables.
class D {
    multi method m (Str $s! is rw) { "rw" }
    multi method m (Str $s!)       { "ro" }
}
is D.m("lit"), "ro", 'literal first: ro candidate';
my Str $e = "y";
is D.m($e),    "rw", 'variable after literal still reaches the rw candidate';

multi sub f(Str $x is rw) { "rw" }
multi sub f(Str $x)       { "ro" }
is f("lit"), "ro", 'sub multi: literal picks non-rw';
my Str $y = "z";
is f($y),    "rw", 'sub multi: variable picks rw';
is f("q"),   "ro", 'sub multi: literal after variable still non-rw';

done-testing;
