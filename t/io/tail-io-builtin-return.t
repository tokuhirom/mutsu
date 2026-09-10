use Test;

# A statement-form I/O builtin (`say`/`put`/`print`/`note`) as the *tail*
# statement of a sub/method/block body is the routine's implicit return value.
# These builtins return True, so the routine returns True — previously the value
# was dropped in sink context and the routine returned Nil/Any.

plan 12;

# Plain sub.
sub s-say  { say "" }
sub s-put  { put "" }
sub s-note { note "" }
ok s-say()  === True, 'sub with `say` tail returns True';
ok s-put()  === True, 'sub with `put` tail returns True';
ok s-note() === True, 'sub with `note` tail returns True';

# Bare block.
my $b = { say "" };
ok $b() === True, 'block with `say` tail returns True';

# Method on both an instance and a type object.
class A { method f { say "" } }
ok A.f     === True, 'method with `say` tail returns True (type object)';
ok A.new.f === True, 'method with `say` tail returns True (instance)';

# multi method.
class B { multi method g { say "" } }
ok B.new.g === True, 'multi method with `say` tail returns True';

# The tail value is usable, e.g. as the condition of the caller.
sub emit-ok { say "emitted" }
ok (emit-ok() ?? "yes" !! "no") eq "yes", 'tail True flows into the caller';

# `say` that is NOT the tail does not become the return value.
sub mixed { say "side"; 42 }
is mixed(), 42, 'a non-tail `say` leaves the real tail value intact';

# A method whose tail is a normal call still returns that call's value.
sub val { 7 }
class C { method h { val() } }
is C.h, 7, 'a non-IO tail call is unaffected';

# print (no newline) also returns True as a tail.
sub s-print { print "" }
ok s-print() === True, 'sub with `print` tail returns True';

# Nested: the block value propagates out through an outer expression.
is (do { say "" }).WHAT.^name, 'Bool', '`do { say }` yields a Bool';
