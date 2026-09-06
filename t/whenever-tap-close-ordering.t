use Test;

# Closing a `whenever`'s Tap must stop FUTURE deliveries only. Values already
# emitted before the `.close` are still delivered, because the emit and the
# close are ordered against each other.
#
# mutsu drops them: `close_whenever` sets a global flag, and
# `Interpreter::drain_waker_events` (`vm/vm_react_subscriptions.rs`) checks that
# flag AFTER `waker.drain()` has already handed it the queued batch -- so every
# event the react loop had not got to yet is discarded, however long before the
# close it was emitted.
#
# Measured 2026-09-06 against raku v2026.07. ADR-0053 owns the design.

plan 4;

# The tap is a real Tap object in both -- ADR-0053's header still says
# "implementation not started", which is stale for this row.
{
    my $s = Supplier.new;
    my $what;
    react {
        my $t = do whenever $s.Supply -> $x { };
        $what = $t.WHAT.^name;
        whenever Promise.in(0.1) { done }
    }
    is $what, 'Tap', 'a `do whenever` in expression position yields a Tap';
}

# No close: every emit is delivered. This is the control.
{
    my $s = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x) };
        $s.emit(1);
        $s.emit(2);
        whenever Promise.in(0.1) { done }
    }
    is @got, [1, 2], 'without a close, every emitted value arrives';
}

# Close AFTER both emits: both must still arrive.
{
    my $s = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x) };
        $s.emit(1);
        $s.emit(2);
        $t.close;
        whenever Promise.in(0.1) { done }
    }
    todo 'the close discards the already-queued batch';
    is @got, [1, 2], 'closing after two emits still delivers both';
}

# Close between: the first arrives, the second does not.
{
    my $s = Supplier.new;
    my @got;
    react {
        my $t = do whenever $s.Supply -> $x { @got.push($x) };
        $s.emit(1);
        $t.close;
        $s.emit(2);
        whenever Promise.in(0.1) { done }
    }
    todo 'ditto -- the value emitted before the close is dropped too';
    is @got, [1], 'closing between two emits delivers only the first';
}
