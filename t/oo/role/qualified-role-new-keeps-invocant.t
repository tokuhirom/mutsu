use Test;

# `self.Role::new(...)` from a class's own `new` runs the role's `new` with
# the class as its invocant. It used to construct the punned role instead, so
# `self` inside it was the role and its `nextsame` re-entered the class's `new`
# until the stack overflowed.

plan 4;

role R {
    method new(*%attrs) {
        die "unknown: %attrs.keys.sort()" if %attrs<bad>:exists;
        self.bless(|%attrs)
    }
    method who { self.^name }
}

class C does R {
    has $.thing;
    method new(*%attrs) {
        %attrs<spy>:delete;
        self.R::new(|%attrs)
    }
}

my $c = C.new(thing => 1, spy => 99);
isa-ok $c, C, 'the qualified role constructor builds the class';
is $c.thing, 1, 'with the forwarded attributes';
dies-ok { C.new(thing => 1, bad => 2) }, "the role's checks still run";

role S { method new() { self.^name } }
class D does S { method new() { self.S::new() } }
is D.new, 'D', "self is the class, not the role, inside the role's new";
