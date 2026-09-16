use Test;

plan 3;

# A private attribute needs `is built`/`is built(:bind)` to be bindable from
# a same-named constructor argument at all (`Type.new(name => ...)` does
# NOT populate a plain `has $!x` -- only a PUBLIC `has $.x` gets that for
# free). mutsu used to drop the `is built` trait entirely when the attribute
# was declared inside a ROLE rather than directly in a class: role-body
# attribute registration never recorded it, and role composition never
# copied it onto the composing class's `attribute_built` table, so a
# private role attribute so declared stayed undefined however it was
# constructed. This is what made mutsu's builtin `X::Wrapper` role (and any
# user role following the same pattern, e.g. `has Mu $!exception is
# required is built(:bind)`) silently fail to capture its constructor
# argument (github.com/tokuhirom/mutsu#8573).

role Holder {
    has Mu $!payload is required is built(:bind);
    method payload { $!payload }
}

class Box does Holder { }

is Box.new(payload => 42).payload, 42, "a role's `is built(:bind)` private attribute binds from the constructor";

role Holder2 {
    has $!plain is built(:bind);
    method plain { $!plain }
}

class Box2 does Holder2 { }

is Box2.new(plain => "x").plain, "x", "a role's plain `is built(:bind)` attribute (no explicit type) binds too";

# A role attribute WITHOUT `is built` must still behave as before: it stays
# undefined even when a same-named constructor argument is passed, matching
# a plain (non-role) private attribute's behaviour.
role NotBuilt {
    has $!unbuilt;
    method unbuilt { $!unbuilt }
}

class Box3 does NotBuilt { }

nok Box3.new(unbuilt => "y").unbuilt.defined, "a role attribute without `is built` stays unbound from a same-named constructor argument";
