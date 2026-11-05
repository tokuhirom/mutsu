use v6;
use Test;

plan 4;

# `&!code`, a private attribute holding code, is carried through finalization as
# the parser built it (ADR-15760 slice 2).

class C {
    has &!f;
    submethod BUILD(:&!f = WhateverCode) {
        without &!f { &!f = -> $n { $n * 2 } }
    }
    method call($x) { &!f($x) }
    method has-code { defined &!f }
}

is C.new.call(4), 8, 'a default assigned through &!f is called';
is C.new(f => { $_ + 1 }).call(4), 5, 'a code argument bound to the attribute is called';
ok C.new.has-code, 'the attribute is defined';

class D {
    has &!g = { 'g' };
    method run { &!g() ~ (&!g).WHAT.^name }
}
is D.new.run, 'gBlock', 'a private code attribute with an initializer';
