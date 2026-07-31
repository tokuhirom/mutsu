use v6;
use Test;

# A role's required (stub) method is satisfied by NAME — rakudo does not
# enforce the stub's signature on the implementation. Pin for
# todo/tickets/cro-bodyserializers-required-method-false-positive.md:
# Cro::HTTP::BodySerializers implements Cro::Core's
# `serialize(Cro::Message $m, $body)` stub with a proto/multi set typed at
# the narrower Cro::HTTP::Message, and mutsu rejected the composition.

plan 8;

class Msg { }
class HttpMsg is Msg { }

role Serializer {
    method serialize(Msg $m, $body --> Str) { ... }
}

role MidSerializer does Serializer {
    method helper() { "h" }
}

# Implementation typed at a NARROWER invocant-arg type than the stub, via
# proto/multi — the exact Cro shape.
class NarrowImpl does MidSerializer {
    proto method serialize(HttpMsg $m, $body --> Str) {*}
    multi method serialize(HttpMsg $m, @body --> Str) { "list" }
    multi method serialize(HttpMsg $m, %body --> Str) { "hash" }
}
is NarrowImpl.new.serialize(HttpMsg.new, {a => 1}), 'hash',
    'narrower-typed proto/multi satisfies a role stub (hash candidate)';
is NarrowImpl.new.serialize(HttpMsg.new, [1, 2]), 'list',
    'narrower-typed proto/multi satisfies a role stub (list candidate)';

# Arity-mismatched plain method also satisfies by name (rakudo parity).
role NeedsArgs {
    method f(Int $x, Str $y --> Str) { ... }
}
class NoArgs does NeedsArgs {
    method f() { "no-args" }
}
is NoArgs.new.f, 'no-args', 'arity-mismatched method still satisfies the stub';

# Inherited method with a different signature satisfies the stub too.
class Base {
    method f(Str $s) { "base($s)" }
}
class Child is Base does NeedsArgs { }
is Child.new.f("x"), 'base(x)', 'inherited differently-signed method satisfies the stub';

# A genuinely missing implementation still errors.
throws-like 'role R2 { method g($x) { ... } }; class C2 does R2 { }; C2.new',
    Exception, message => /'must be implemented'/,
    'missing implementation still fails composition';

# A stubbed MULTI keeps per-candidate signature enforcement (unlike a plain
# method stub): `multi method m(Int) { ... }` is satisfied only by a
# signature-matching candidate (S14-roles/stubs.t pins the roast side).
role MultiStub {
    multi method m(Int) { ... }
}
lives-ok { EVAL 'class MultiOk does MultiStub { multi method m(Int) { "int" } }' },
    'matching multi satisfies a stubbed multi';
dies-ok { EVAL 'class MultiBad does MultiStub { multi method m(Str) { "str" } }' },
    'non-matching multi does NOT satisfy a stubbed multi';

# A stub satisfied by an exact match keeps working.
role Exact {
    method e($x) { ... }
}
class ExactImpl does Exact {
    method e($x) { "e($x)" }
}
is ExactImpl.new.e(1), 'e(1)', 'exact-signature satisfaction unchanged';

done-testing;
