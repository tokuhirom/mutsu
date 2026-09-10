use Test;

# A private method call `self!name` must accept apostrophes inside the
# identifier, matching Raku's identifier rules (hyphens and apostrophes are
# allowed between word segments). Regression: `self!rotate-left'right` used to
# fail to parse because the private-method-name scanner only allowed hyphens.
# (AVL-Tree dist uses `rotate-left'right` / `rotate-right'left`.)

plan 4;

class C {
    method !foo-x'y($n) { $n * 2 }
    method call-colon() { self!foo-x'y: 5 }
    method call-paren() { self!foo-x'y(7) }
}

is C.new.call-colon, 10, "private call with apostrophe name (colon args)";
is C.new.call-paren, 14, "private call with apostrophe name (paren args)";

# Apostrophe only counts when followed by a letter; a trailing quote still ends
# the name. `self!foo` followed by a quoted string arg must not be swallowed.
class D {
    method !foo($n) { $n }
    method go() { self!foo('ok') }
}
is D.new.go, 'ok', "apostrophe not followed by letter ends the method name";

# Mixed hyphen + apostrophe segments.
class E {
    method !rotate-left'right($x) { "L$x" }
    method !rotate-right'left($x) { "R$x" }
    method both() { self!rotate-left'right(1) ~ self!rotate-right'left(2) }
}
is E.new.both, "L1R2", "multiple apostrophe/hyphen private method names";
