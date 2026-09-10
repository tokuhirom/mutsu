use Test;

plan 9;

# Pins the fix for todo/tickets/repeat-call-loses-backtrace-frame.md.
#
# The FIRST call to a routine is dispatched through the frame-pushing named
# call path, which correctly pushes a `RoutineFrame` for backtraces. A
# repeat call to the SAME call site is routed through
# `call_compiled_function_fast` (see src/vm/vm_call_fast.rs), which used to
# skip pushing a `RoutineFrame` entirely to avoid per-call `String`
# allocation on this hot path. That silently dropped the `in sub f`
# backtrace frame on the SECOND and later call to the same routine, and the
# one remaining frame reported the wrong line (the `sub` declaration's line
# instead of the actual call site) -- because the whole backtrace is built
# from the live routine stack at the moment of the error, and that stack
# never had the frame to begin with.
#
# This test calls the same failing routine 3 times in a loop and asserts
# every call's backtrace has the routine frame with the correct call-site
# line. A single call would not catch the regression -- only a REPEAT call
# takes the fast path that lost the frame.

sub f() { die "boom" }              # line 24

for 1..3 -> $i {
    try { f() };                    # line 27 -- the call site
    my $bt = $!.backtrace;
    my @f-frames = $bt.list.grep({ .is-routine && .subname eq 'f' });
    ok @f-frames.elems >= 1, "call $i: backtrace has an 'f' routine frame";
    is @f-frames[0].line, 24, "call $i: 'f' frame reports the die's line";
    my $bt-str = $bt.Str;
    ok $bt-str.contains('line 27'),
        "call $i: enclosing frame reports the call site (27), not a stale line";
}
