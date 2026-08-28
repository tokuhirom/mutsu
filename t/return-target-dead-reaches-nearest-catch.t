use v6;
use Test;

# A `return` whose lexically-captured target routine has already exited the
# dynamic call stack (a "dead" return) must convert into a catchable
# `X::ControlFlow::Return` and be delivered to the NEAREST enclosing
# `try`/`CATCH` -- not silently swallowed by an unrelated routine call frame
# in between, and not left to blow straight past every `try`/`CATCH` on its
# way to the top level.
#
# mutsu's `try`/CATCH dispatch (`vm_try_catch_ops.rs`) always propagated
# `is_return()` past itself unconditionally -- correct for a LIVE return
# (still hunting for its target further up the stack) but wrong for a dead
# one, which can never be caught by unwinding further. And a `return` forced
# while reifying a lazy `gather` (via `.Str`/`~`, sink, or any other forcing
# path) never had its target callable id resolved at all, so it fell back to
# the "untargeted return -> nearest enclosing routine catches it as its OWN
# return" rule -- silently truncating an unrelated caller instead of ever
# reaching a CATCH. Two separate root causes, same observable symptom.

plan 9;

# Case 0: a mainline `try` (no enclosing routine at all) around a dead
# closure return.
{
    sub a1 { my &x = { return }; &x }
    my &y = a1;
    my $e;
    try { &y(); CATCH { default { $e = $_ } } }
    is $e.^name, 'X::ControlFlow::Return',
        'a dead closure return reaches a mainline try/CATCH';
    ok $e.out-of-dynamic-scope, 'and carries out-of-dynamic-scope';
}

# Case 1: the same dead return, but forced from inside a bare block with its
# own CATCH (not a `try` keyword) -- the shape a `{ ...; CATCH {...} }`
# implicit-CATCH block compiles to.
{
    sub a2 { my &x = { return }; &x }
    my &y2 = a2;
    my $caught;
    {
        &y2();
        CATCH {
            default { $caught = $_.^name }
        }
    }
    is $caught, 'X::ControlFlow::Return',
        'a dead closure return reaches a bare block CATCH';
}

# Case 2: the return is dead, but an UNRELATED routine (`call-it`) is on the
# dynamic stack between the mainline and the closure call. That routine must
# NOT silently "catch" the return as its own -- the signal must keep
# propagating until it reaches call-it's own CATCH (or further).
{
    sub call-it(&code) {
        code();
        CATCH { default { return 'caught:' ~ $_.^name } }
        'fell-through-uncaught';
    }
    sub a3 { my &x = { return }; &x }
    my &y3 = a3;
    is call-it({ &y3() }), 'caught:X::ControlFlow::Return',
        'a dead closure return is not silently absorbed by an unrelated caller';
}

# Case 3: a `return` inside a `gather` body, forced (via `.Str`) long after
# the routine that wrote the gather has exited, through an unrelated caller
# exactly like case 2 -- this is the shape the real `Test.rakumod`'s
# `subtest(&subtests) { subtests(); CATCH {...} }` exercises.
{
    sub call-it2(&code) {
        code();
        CATCH { default { return 'caught:' ~ $_.^name } }
        'fell-through-uncaught';
    }
    my sub f() { gather { return } }
    is call-it2({ ~f() }), 'caught:X::ControlFlow::Return',
        'a dead gather-forced return is not silently absorbed by an unrelated caller';
}

# Case 4: same as case 3 but forced via a bare discarded statement (the
# `SinkPop` opcode path) rather than `.Str`, exercised through a `try` this
# time. (An explicit `.sink()` METHOD call on a gather-based LazyList has its
# own, separate bug -- it never runs the body at all, see
# `todo/tickets/lazylist-sink-method-does-not-force-gather-body.md` -- so
# this case deliberately uses a bare statement, not `.sink()`.)
{
    my sub f2() { gather { return } }
    my $e;
    try { f2(); CATCH { default { $e = $_ } } }
    is $e.^name, 'X::ControlFlow::Return',
        'a dead gather-forced return reaches CATCH when sunk';
}

# Control case: a LIVE return (its target routine is still on the dynamic
# stack) must NOT be converted -- it keeps propagating past an intervening
# try/CATCH exactly as before, and actually returns from its target.
{
    sub outer {
        my &x = { return 42 };
        try { x() };
        return 'unreached';
    }
    is outer(), 42, 'a live closure return still propagates past an intervening try';
}

# Control case: a LIVE gather-forced return also still propagates correctly.
{
    sub outer2 {
        my $s = gather { return 99 };
        try { ~$s };
        return 'unreached';
    }
    is outer2(), 99, 'a live gather-forced return still propagates past an intervening try';
}

# Control case: an UNTARGETED bare top-level `return`-like EVAL still throws
# X::ControlFlow::Return cleanly (regression guard for the None-target arm).
{
    my $e;
    try { EVAL 'return 1'; CATCH { default { $e = $_ } } }
    is $e.^name, 'X::ControlFlow::Return',
        'a bare untargeted return still converts cleanly';
}
