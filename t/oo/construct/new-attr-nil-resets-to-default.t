use Test;

# #9676: `Class.new(:attr(Nil))` used to leave the attribute holding the raw
# `Nil` instead of resetting it to the container's declared type default —
# exactly what a plain `$x = Nil` assignment already did correctly. Rakudo
# treats a constructor-supplied `Nil` the same as any other scalar-container
# `Nil` assignment (BUILDALL stores through the attribute the same way an
# ordinary `=` would).

plan 9;

class C {
    has $.m;
    has $.d = 5;
    has Int $.t;
}

is C.new(:m(Nil)).m.raku, 'Any', 'untyped attribute: constructor Nil resets to Any';
is C.new(:d(Nil)).d.raku, 'Any',
    'defaulted attribute: constructor Nil resets to Any, not the = 5 initializer';
is C.new(:t(Nil)).t.raku, 'Int', 'typed attribute: constructor Nil resets to its type object';

# The same reset applies through .bless directly, and through a custom `new`
# that forwards to it -- both share the named-arg-to-attribute path.
is C.bless(:m(Nil)).m.raku, 'Any', '.bless: constructor Nil resets to Any';
is C.bless(:t(Nil)).t.raku, 'Int', '.bless: typed attribute Nil resets to its type object';

class D {
    has $.x;
    method new(*%args) { self.bless(|%args) }
}
is D.new(:x(Nil)).x.raku, 'Any', 'custom new via self.bless(|%args): constructor Nil resets to Any';

# A provided non-Nil value is untouched.
is C.new(:m(42)).m, 42, 'a provided non-Nil value is stored as-is';
is C.new(:t(42)).t, 42, 'a provided non-Nil typed value is stored as-is';

# No-arg construction is unaffected by the fix.
is C.new.m.raku, 'Any', 'no constructor argument still seeds the nominal type object';
