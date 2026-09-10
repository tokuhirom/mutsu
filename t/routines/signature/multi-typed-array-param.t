use v6;
use Test;

# Multi dispatch on a typed array parameter with a definedness smiley:
# `Field:D @fld` accepts a `my Field @f` argument — the :D constrains the
# ELEMENTS, not the declared element type object, so the declared-type
# comparison must strip it (Text::CSV's `multi method string`).

plan 4;

class Field { has $.v }

multi sub g (--> Str) { my Field @f; g(@f) }
multi sub g (Field:D @fld --> Str) { "sub got " ~ @fld.elems }

is g(), "sub got 0", 'sub multi: empty typed array picks the @-candidate';

class C {
    multi method string (--> Str) {
        my Field @f;
        self.string(@f);
    }
    multi method string (Field:D @fld --> Str) {
        "got " ~ @fld.elems;
    }
}

my Field @e;
is C.new.string(@e), "got 0", 'method multi: empty typed array from caller';
is C.new.string, "got 0", 'method multi: internal re-dispatch with empty typed array';

my Field @g2 = Field.new(:v(1)), Field.new(:v(2));
is C.new.string(@g2), "got 2", 'method multi: populated typed array';
