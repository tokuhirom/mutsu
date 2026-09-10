use Test;

plan 6;

# A `supply { emit ...; die }` block delivers the emits that ran before the
# die, then runs the quit callback. The tap callback's captured-outer scalar
# writes (e.g. `$emits-run++`) must propagate back to the caller even though
# dispatching the quit callback clears the pending rw-writeback sources.
{
    my $s = supply {
        emit 'onions';
        die 'poop';
    }

    my $emits-run = 0;
    my $dones-run = 0;
    my $quits-run = 0;
    $s.tap({ $emits-run++ }, done => { $dones-run++ }, quit => { $quits-run++ });
    is $emits-run, 1, 'emit before die runs emit subscription once';
    is $dones-run, 0, 'die never runs done subscription';
    is $quits-run, 1, 'die runs quit subscription once';
}

# Two emits before a die: both captured-outer increments propagate.
{
    my $s = supply {
        emit 'a';
        emit 'b';
        die 'x';
    }

    my $emits-run = 0;
    my $quits-run = 0;
    $s.tap({ $emits-run++ }, quit => { $quits-run++ });
    is $emits-run, 2, 'both emits before die propagate to caller scalar';
    is $quits-run, 1, 'quit callback ran once';
}

# Sanity: no die, captured-outer write still propagates (unchanged behavior).
{
    my $s = supply { emit 'a'; emit 'b'; }
    my $emits-run = 0;
    $s.tap({ $emits-run++ });
    is $emits-run, 2, 'emits without die propagate';
}
