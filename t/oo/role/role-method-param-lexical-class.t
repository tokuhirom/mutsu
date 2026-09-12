use Test;

# A lexically-scoped `my class` / `my role` registers under a mangled storage
# key (ADR-0047 P1, `Name\0<decl-id>`) with `env` binding the bare name to it.
# `is_resolvable_type` probed the registry keys directly, so it never saw the
# type under the spelling a signature writes -- and only the ROLE-method
# validator noticed: the sub pre-pass accepts such a name out of
# `declared_types`, the unit's statically gathered declarations, which a role
# body has no equivalent of.
#
# From `Protocol::MQTT`, which declares `my class EncodeBuffer { ... }` at file
# scope and names it in `our role Packet[...]`:
#
#     method !encode-body(Packet:D: EncodeBuffer $buffer --> Nil) { ... }
#
# https://github.com/tokuhirom/mutsu/issues/7993

plan 6;

my class EncodeBuffer {
    has $.tag = 'buf';
}

my role Marker { }

# A sub and a class method already accepted this; the role method did not.
sub takes-buf(EncodeBuffer $b) { $b.tag }
is takes-buf(EncodeBuffer.new), 'buf', 'a sub parameter accepts a `my class`';

class Holder {
    method show(EncodeBuffer $b) { $b.tag }
}
is Holder.show(EncodeBuffer.new), 'buf', 'a class method parameter accepts a `my class`';

role Packet {
    method encode(EncodeBuffer $b) { $b.tag }
}
class Concrete does Packet { }
is Concrete.encode(EncodeBuffer.new), 'buf', 'a role method parameter accepts a `my class`';

# The constraint is a real type check, not a name waved through.
dies-ok { Concrete.encode(42) }, 'the `my class` constraint still rejects a bad argument';

# A `my role` named in a role method signature resolves the same way.
role UsesMarker {
    method takes(Marker $m) { 'marked' }
}
class MarkerC does UsesMarker { }
my class DoesMarker does Marker { }
is MarkerC.takes(DoesMarker.new), 'marked', 'a role method parameter accepts a `my role`';

# Guard: an undeclared name is still rejected, so the fix did not turn the
# validator into a rubber stamp.
throws-like 'role Bogus { method m(NoSuchTypeHere $x) { $x } }; class BogusC does Bogus { }',
    X::Parameter::InvalidType,
    'an undeclared typename is still reported';
