use Test;

# A `whenever` that taps an on-demand supply from inside a running react
# (`whenever $outer { whenever $sod { } }`) must fire the on-demand supply's
# `closing => { ... }` callback on the react thread, so a write to a captured
# react-block lexical is not lost — even when the supply body is an async
# `start { emit; done }`. Regression: roast/S17-supply/syntax.t test 75
# ("Supply is closed by Supply block after it sends done").
#
# It also verifies a bare `whenever $sod { }` statement does NOT clobber `$sod`
# with its Tap (which would make the next tap a no-op): the compiler only
# bridges the tap out through the supply var for `do whenever` expressions.
#
# Exit deterministically once `closing` has fired at least once (`done if
# $closed`) instead of racing a fixed `Promise.in(0.4)` timer: under load the
# interval timer's first tick could slip past the 0.4s window (0 ticks -> the
# 0.4s timer wins -> closed=0), which made subtest 2 flaky in CI. A generous
# 5s backstop keeps a genuine regression (closing never fires) failing cleanly
# with closed=0 rather than hanging.

plan 3;

# Async body: closing fires per tap, incrementing the captured lexical.
{
    my $closed = 0;
    my $sod = Supply.on-demand:
        -> $s { start { $s.emit(42); $s.done; } },
        closing => { $closed++ };
    react {
        whenever Supply.interval(0.02) {
            whenever $sod { }
            done if $closed;
        }
        whenever Promise.in(5) { done }
    }
    ok $closed, "async on-demand closing fires from a nested whenever (closed=$closed)";
}

# Synchronous body: closing fires per tap.
{
    my $closed = 0;
    my $sod = Supply.on-demand:
        -> $s { $s.emit(1); $s.done; },
        closing => { $closed++ };
    react {
        whenever Supply.interval(0.02) {
            whenever $sod { }
            done if $closed;
        }
        whenever Promise.in(5) { done }
    }
    ok $closed, "sync on-demand closing fires from a nested whenever (closed=$closed)";
}

# A bare `whenever $sup { }` statement must NOT clobber `$sup` with its Tap: the
# supply variable stays a Supply and can be tapped again. (The `do whenever`
# expression bridge is covered by t/react-do-whenever-tap-coherence.t.)
{
    my $sup = Supplier.new.Supply;
    react {
        whenever $sup { }
        isa-ok $sup, Supply, "bare whenever does not clobber the supply var";
        done;
    }
}
