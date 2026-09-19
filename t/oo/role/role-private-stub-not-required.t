use Test;

# A role's PRIVATE stub method (`method !foo { ... }`) is never a composition
# requirement, unlike a public one. Private methods are not virtual in Raku --
# `self!foo` always resolves to the role's OWN `!foo`, never to a composing
# class's method of the same name -- so a class cannot "implement" one, and
# Rakudo raises no error at composition time even when nothing anywhere ever
# defines it (verified against `raku`; it would only die "Stub code executed"
# if the role's own body called it). #8806's real-world trigger:
# Math::Matrix::Util stubs a private `!clone-rows` that Math::Matrix never
# implements and Math::Matrix::Util's own body never calls.

plan 3;

dies-ok { EVAL 'role PublicStub { method foo { ... } }; class A does PublicStub { }' },
    'a public role stub with no implementation still dies (unchanged)';

lives-ok {
    EVAL 'role PrivateStub { method !foo { ... } }; class B does PrivateStub { method other { "ok" } }';
}, 'a private role stub with no implementation anywhere does not raise a composition error';

is EVAL('
    role PrivateStub2 { method !foo { ... } }
    class C does PrivateStub2 { method other { "ok" } }
    C.new.other
'), "ok", 'the composing class works normally once composed';
