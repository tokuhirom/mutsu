use Test;

# #18608: `.delayed` over a live source hands each value to the scheduler, so
# the emitter returns at once; the taps see the values in order after the
# delay, and `done` follows the last pending value.

plan 7;

{
    my $s = Supplier.new;
    my @b;
    $s.Supply.delayed(0.2).tap({ @b.push: $_ });
    my $t0 = now;
    $s.emit(5);
    $s.emit(6);
    ok now - $t0 < 0.15, 'emit does not wait for the delay';
    is @b.elems, 0, 'nothing is delivered before the delay';
    sleep 0.6;
    is-deeply @b, [5, 6], 'the values arrive in order after the delay';
}

{
    my $s = Supplier.new;
    my @b;
    my $done = Promise.new;
    $s.Supply.delayed(0.1).tap({ @b.push: $_ }, done => { $done.keep(@b.elems) });
    $s.emit($_) for 1..3;
    $s.done;
    is @b.elems, 0, 'done does not flush the pending values at once';
    is await($done), 3, 'done runs after every pending value';
}

{
    my $s = Supplier.new;
    my $d = $s.Supply.delayed(0.1);
    my (@a, @b);
    $d.tap({ @a.push: $_ });
    $d.tap({ @b.push: $_ });
    $s.emit(1);
    sleep 0.4;
    is-deeply (@a, @b), ([1], [1]), 'each tap gets the delayed value';
}

{
    my $s = Supplier.new;
    my @got;
    react {
        whenever $s.Supply.delayed(0.05) { @got.push: $_; done if $_ == 2 }
        $s.emit(1);
        $s.emit(2);
    }
    is-deeply @got, [1, 2], 'a react block over a delayed live supply';
}
