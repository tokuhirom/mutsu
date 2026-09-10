use Test;

plan 4;

# Regression test: `my $x := @a[$i]` inside a loop body must bind a fresh
# ContainerRef cell to the CURRENT array element on every iteration. A
# compiler bug reused the `is rw` call-argument writeback machinery (meant
# for `f(@a[$i])` where `f` has an `is rw` param) for `:=` bind targets too.
# Those writeback temps are compile-time-fixed global names; inside a loop
# the same bind statement re-executes every iteration, so on the 2nd+
# iteration the writeback's "write through an existing ContainerRef" wrongly
# wrote into the *previous* iteration's bound cell instead of storing a
# fresh reference, corrupting earlier array elements.

{
    my @out = 0 xx 5;
    for 0..4 -> $i {
        my $slot := @out[$i];
        $slot = ($i + 1) * 10;
    }
    is @out, [10, 20, 30, 40, 50], 'for-loop: my $x := @a[$i] binds a fresh cell per iteration';
}

{
    my @out = 0 xx 5;
    loop (my $i = 0; $i < 5; $i++) {
        my $slot := @out[$i];
        $slot = ($i + 1) * 10;
    }
    is @out, [10, 20, 30, 40, 50], 'C-style loop: my $x := @a[$i] binds a fresh cell per iteration';
}

{
    # Two distinct bindings per iteration (source + destination), matching
    # the pattern in roast/S17-lowlevel/lock.t.
    my @in = 1..5;
    my @out = 0 xx 5;
    loop (my $i = 0; $i < @in; $i++) {
        my $in := @in[$i];
        my $out := @out[$i];
        $out = $in * 10;
    }
    is @out, [10, 20, 30, 40, 50], 'loop with two element binds per iteration stays isolated';
}

{
    my @out = 0 xx 3;
    for 0..2 -> $i {
        my $slot := @out[$i];
        $slot = $i;
    }
    is @out, [0, 1, 2], 'bound value 0 does not get lost on later iterations';
}
