use Test;

# A method whose body has no closures does not write `self`, `?CLASS` and
# `?ROLE` into its frame env (ADR-14633); the runtime helpers it calls still
# have to see its own invocant, class and role -- not a caller's.

plan 12;

role R { method who { ::?ROLE.^name } }

class Plain {
    has $.v = 3;
    method name { self.^name }
    method cls { ::?CLASS.^name }
    method val { $!v }
    method same(Plain $o) { self === $o }
    method nested { self.name ~ '/' ~ self.cls }
}

class WithClosure {
    has $.tag = 'wc';
    # A closure in the body keeps the frame writing its keys.
    method who { my &f = { self.tag }; f() ~ ':' ~ Plain.new.name }
    method call-plain(Plain $p) { $p.name ~ '+' ~ self.^name }
}

class Child is Plain does R { }

my $p = Plain.new;
is $p.name, 'Plain', 'self.^name in a closure-free method';
is $p.cls, 'Plain', '::?CLASS in a closure-free method';
is $p.val, 3, 'attribute read';
ok $p.same($p), 'self identity';
is $p.nested, 'Plain/Plain', 'a method calling methods on self';

my $c = Child.new;
is $c.name, 'Child', 'an inherited method answers the instance class';
is $c.cls, 'Plain', '::?CLASS is the declaring class of an inherited method';
is $c.who, 'R', '::?ROLE inside a role method';

# A closure-free method called from a method that writes its keys must not
# see the caller's self.
my $w = WithClosure.new;
is $w.who, 'wc:Plain', 'a closure-free callee under a closure-writing caller';
is $w.call-plain($p), 'Plain+WithClosure', 'callee and caller each see their own self';

# And the other way round: key-writing callee under closure-free caller.
class Outer {
    method go { WithClosure.new.who ~ '|' ~ self.^name }
}
is Outer.new.go, 'wc:Plain|Outer', 'a closure-writing callee under a closure-free caller';

# A role method called on a class instance does not leak ?ROLE to a plain one.
class Mixed does R {
    method both { self.who ~ '/' ~ Plain.new.cls }
}
is Mixed.new.both, 'R/Plain', '?ROLE of a role method does not leak into a plain method';
