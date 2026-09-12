use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# An END phaser holds a *copy* of the scope it closed over. When a closure
# mutates a captured lexical whose declaring frame dies before exit, that copy
# is the phaser's only surviving binding, so the closure's return has to
# refresh it (`update_end_phaser_envs_for_keys`).
#
# #7565: that refresh used to walk the whole capture on EVERY closure return —
# a set whose width is the *creating scope's*, so `use Test` alone made it
# hundreds of names — and to re-store every one of them even when the value was
# the very same binding it already held. It now considers only the names the
# call could have changed. These pin the cases that narrowing must keep.

plan 6;

is_run 'my $x = 1; END { say $x }; my &c = { $x = 42 }; c();',
    { :out("42\n") },
    'a closure writing a captured mainline lexical refreshes the END capture';

is_run 'sub f() { my $x = 1; END { say $x }; my &c = { $x = 42 }; c() }; f();',
    { :out("42\n") },
    'and when the declaring frame dies before exit, so the copy is all there is';

# The write is made by a *called* routine, not by the closure body itself, so
# no compile-time free-variable set of the closure names it: only the runtime
# writeback does.
is_run 'sub f() { my $x = 1; END { say $x }; sub w() { $x = 7 }; my &c = { w() }; c() }; f();',
    { :out("7\n") },
    'a write made through a call inside the closure still reaches the capture';

# A closure that writes nothing must not drag an unrelated live value into a
# phaser's capture — and the phaser must still read the mainline's final value,
# which it gets from the live exit-time env rather than from its copy.
is_run 'my $x = 1; END { say $x }; my &c = { 0 }; $x = 5; c();',
    { :out("5\n") },
    'a non-writing closure leaves the mainline value to the live exit env';

# A same-named lexical in a sibling scope must never clobber the phaser's own
# captured binding.
is_run 'sub f() { my $a = 1; END { say $a } }; f(); my $a = 99; my &c = { $a }; c();',
    { :out("1\n") },
    'an unrelated same-named capture does not overwrite a frozen phaser key';

# Repeated calls keep tracking: the refresh is not a once-only latch.
is_run 'sub f() { my $n = 0; END { say $n }; my &c = { $n = $n + 1 }; c(); c(); c() }; f();',
    { :out("3\n") },
    'every call refreshes, so the last write is the one the phaser sees';
