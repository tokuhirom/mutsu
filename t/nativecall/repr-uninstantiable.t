use Test;
use nqp;

# `is repr<Uninstantiable>` (upstream NativeCall's `class void`) reports that
# REPR and has no instances. Upstream's check_routine_sanity follows
# `Pointer.of` to `void` and accepts the parameter only on that REPR (#11209).

plan 5;

class V is repr<Uninstantiable> { }
class Plain { }

is V.REPR, 'Uninstantiable', 'the declared REPR is reported';
is Plain.REPR, 'P6opaque', 'an ordinary class is unaffected';
throws-like { V.new }, Exception,
    message => 'You cannot create an instance of this type (V)', '.new dies';
dies-ok { nqp::create(V) }, 'nqp::create dies';
ok V.^can('gist'), 'the type object itself is usable';
