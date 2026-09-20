# ADR-0110 Stage 2: what a TRIR body's generic call (`CallGen`) must hand the
# ordinary dispatch, and in which package it must resolve the name.
#
# A TRIR frame is not a `RoutineFrame` and its slots are not the caller's
# locals, so two things the untyped call site gets for free have to be
# supplied explicitly: the routine's own declaring package, and the `VarRef`
# shape a `WrapVarRef` emits. Each `is` below failed before that was done.
#
# The pattern that matters is calling the SAME routine twice: the first call
# runs untyped (the call site is linked to TRIR only after it has executed),
# so a bug in the TRIR path shows on the second call and not the first.
use Test;

plan 12;

# --- the declaring package ---------------------------------------------------
# `mm` is package-scoped, so resolving it needs `current_package` to be `C`.
# Without that the second call reported `Unknown function: mm`.
module C {
    multi sub mm(Int $x) { "mm-int" }
    multi sub mm(Str $x) { "mm-str" }
    our sub call-mm($v) { mm($v) }
}
is C::call-mm(1), 'mm-int', 'a package-scoped multi resolves from a TRIR body';
is C::call-mm(2), 'mm-int', '... on the second call, which is the TRIR one';
is C::call-mm('x'), 'mm-str', '... and the other candidate still wins';

module P {
    proto sub pp($) {*};
    our multi sub pp(Int $x) { "pp-int" }
    our multi sub pp(Str $x) { "pp-str" }
    our sub call-pp($v) { pp($v) }
}
is P::call-pp(1), 'pp-int', 'a package-scoped proto resolves from a TRIR body';
is P::call-pp(2), 'pp-int', '... on the second call too';
is P::call-pp('x'), 'pp-str', '... and dispatches on the argument';

# --- the argument shape ------------------------------------------------------
# A `proto`'s `{*}` stashes the caller's argument list and replays it to the
# winning candidate verbatim, so a TRIR generic call's container argument
# reached the candidate's type check as itself and failed its own constraint
# ("expected Str, got Str").
proto sub p1($) {*};
multi sub p1(Str $x) { "p1-str" }
sub c5($v) { p1($v) }
is c5('a'), 'p1-str', 'a container argument satisfies a proto candidate';
is c5('b'), 'p1-str', '... on the TRIR call as well as the first';

proto sub p2($) {*};
multi sub p2(Int $x) { "p2-int" }
sub c6($v) { p2($v) }
is c6(1), 'p2-int', '... for a native-shaped constraint too';
is c6(2), 'p2-int', '... twice';

# --- and the write-back that the container is there for ----------------------
# The container is what lets a generic callee's `is rw` parameter write back
# into the TRIR frame's slot, so it cannot simply be dropped.
sub gen-bump($pos is rw) { $pos = $pos + 7 }
sub via-gen(int $pos is rw --> Nil) { gen-bump($pos) }
my int $g = 1;
via-gen($g);
is $g, 8, 'a generic callee writes its `is rw` argument back into the frame';
$g = 10;
via-gen($g);
is $g, 17, '... on the TRIR call too';
