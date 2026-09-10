use v6;
use Test;

# `MarkBindContext` (and its sibling one-shot VM flags: scalar_bind_context,
# rebind_context, constant_context, array_share_context, vardecl_context,
# explicit_initializer_context, param_raw_bind_context) is set right before a
# `:=` bind target's store op, meant to be consumed by the VERY NEXT
# SetLocal/SetGlobal. When a real function/method CALL sits between the mark
# and its consumer (`@!other := make();` compiles to `MarkBindContext; ...;
# CallFuncNamed; ...; SetGlobal`), the callee's OWN body used to run with the
# flag still set, so ANY vardecl/store inside the callee (e.g. `my uint8
# @state = 0..255;`) was wrongly treated as a bind target too, skipping the
# Range-to-array materialization a typed native array needs -- leaving a bare
# immutable Range where a mutable typed array was expected. Blocked
# Crypt::RC4's dist test suite. See
# todo/deep/mark-context-flags-leak-across-live-call-boundary.md.

plan 4;

# Minimal repro: an attribute bind through an ordinary named-sub call.
class Foo {
    has uint8 @!other;
    method go() {
        @!other := make();
    }
    method other() { @!other }
}
sub make() {
    my uint8 @state = 0..5;
    @state[2] = 99;
    @state;
}
my $f = Foo.new;
$f.go;
is $f.other.join(','), '0,1,99,3,4,5',
    'attribute bind through a sub-call boundary does not leak bind-context into the callee body';

# The bind target itself is still a real bind (same container), not a copy --
# the isolation must not break the intended MarkBindContext consumer.
class Bar {
    has uint8 @!other;
    method go() {
        @!other := make2();
    }
    method other() { @!other }
}
sub make2() {
    my uint8 @state = 0..5;
    @state;
}
my $b = Bar.new;
$b.go;
is $b.other.of.raku, 'uint8', 'the outer bind itself still preserves the callee typed-array shape';

# Method-call boundary (call_compiled_closure_with_topic path): the callee is
# itself a method, not a plain sub.
class Baz {
    has uint8 @!other;
    method go() {
        @!other := self.make3();
    }
    method make3() {
        my uint8 @state = 0..5;
        @state[2] = 99;
        @state;
    }
    method other() { @!other }
}
my $z = Baz.new;
$z.go;
is $z.other.join(','), '0,1,99,3,4,5',
    'attribute bind through a method-call boundary does not leak bind-context into the callee body';

# A chain of nested calls between the mark and its consumer must isolate at
# every level, not just the immediate callee.
class Qux {
    has uint8 @!other;
    method go() {
        @!other := outer();
    }
    method other() { @!other }
}
sub outer() {
    inner();
}
sub inner() {
    my uint8 @state = 0..5;
    @state[2] = 99;
    @state;
}
my $q = Qux.new;
$q.go;
is $q.other.join(','), '0,1,99,3,4,5',
    'a chain of nested calls between the mark and its consumer isolates at every level';
