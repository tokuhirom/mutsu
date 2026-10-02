use Test;

# A `whenever` on a `supply { whenever $live { emit ... } }` fires its LAST
# phaser when that supply is done -- i.e. when the inner live source completes
# -- not as soon as the supply block's body has run. It used to fire at
# subscription time, before any value arrived, so a `LAST done` ended the
# react with nothing received.

plan 5;

{
    my $sup = Supplier.new;
    my $s = supply { whenever $sup.Supply { emit $_ * 10 } };
    my @events;
    start { sleep .2; $sup.emit(3); $sup.emit(4); $sup.done }
    react {
        whenever $s { @events.push: $_; LAST @events.push: 'LAST' }
    }
    is-deeply @events, [30, 40, 'LAST'], 'LAST fires after the values, once the inner source is done';
}

{
    my $sup = Supplier.new;
    my $s = supply { whenever $sup.Supply { emit $_ } };
    my @got;
    my $timed-out = False;
    start { sleep .2; $sup.emit('x'); $sup.done }
    react {
        whenever $s { @got.push: $_; LAST done }
        whenever Promise.in(10) { $timed-out = True; done }
    }
    is-deeply @got, ['x'], 'LAST done does not end the react before the value arrives';
    nok $timed-out, 'and the react ends on LAST, not on the timeout';
}

{
    my $sup = Supplier.new;
    my $s = supply { whenever $sup.Supply { emit $_ } };
    my $last-count = 0;
    start { sleep .1; $sup.emit(1); $sup.done }
    react {
        whenever $s { LAST $last-count++ }
    }
    is $last-count, 1, 'LAST fires exactly once';
}

{
    # A synchronous supply body still fires LAST right after its values.
    my @events;
    react {
        whenever supply { emit 1; emit 2 } { @events.push: $_; LAST @events.push: 'LAST' }
    }
    is-deeply @events, [1, 2, 'LAST'], 'a synchronous supply block is unchanged';
}
