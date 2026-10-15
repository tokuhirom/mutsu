use Test;

plan 10;

# A refined multi method candidate (`where`, `subset`, `UInt`) carries rakudo's
# bind-check flag: it ties a named-only candidate that has no positional (the
# first declared wins), skips the optional-positional step, and a refined tie
# is trial-bound, never ambiguous (#13522; the sub side is #13523).

class C {
    multi method a(:$x = 1) { 'named-only' }
    multi method a(UInt $s = 1, :$x = 1) { 'pos-optional' }
    multi method b(UInt $s, $t?) { 'opt' }
    multi method b(UInt $s) { 'noopt' }
}
is C.a, 'named-only', 'a refined optional-positional candidate ties a named-only one: first declared';
is C.b(5), 'opt', 'a refined candidate skips the optional-positional step';

class D {
    multi method a(UInt $s = 1, :$x = 1) { 'pos-optional' }
    multi method a(:$x = 1) { 'named-only' }
}
is D.a, 'pos-optional', 'the same pair declared the other way round';

class E {
    multi method c(UInt $s = 1) { 'refined' }
    multi method c() { 'bare' }
}
is E.c, 'refined', 'a refined candidate still beats a bare one with nothing to tie on';

class F {
    multi method g(UInt $x) { 'first' }
    multi method g(UInt $x) { 'second' }
}
is F.g(1), 'first', 'two identical refined candidates are not ambiguous';

class G {
    multi method h(Int $x where * > 0) { 'positive' }
    multi method h(Int $x) { 'any' }
}
is G.h(5), 'positive', 'a refinement on a shared positional still wins';
is G.h(-5), 'any', 'and falls back when it does not bind';

class H {
    multi method i(Int $x, Int $y?) { 'two' }
    multi method i(Int $x) { 'one' }
}
is H.i(1), 'one', 'without a refinement the omitted optional still ranks wider';

role R { multi method j(UInt $s = 1, :$x = 1) { 'role-refined' } }
class I does R { multi method j(:$x = 1) { 'class-named' } }
is I.j, 'class-named', "the class's own candidate is declared first";

class J {
    multi method k(UInt $s, :$x) { 'refined-named' }
    multi method k(UInt $s) { 'refined' }
}
is J.k(1), 'refined-named', 'a refined tie: the first declared that binds';
