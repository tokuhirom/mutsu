use Test;

# #18696: `.delayed` over a finite supply cues each value on the scheduler,
# so `.tap` returns at once; every tap replays the source into its own delay,
# and `done` follows the last value.

plan 7;

{
    my @b;
    my $t0 = now;
    Supply.from-list(1, 2, 3).delayed(0.2).tap({ @b.push: $_ });
    ok now - $t0 < 0.15, '.tap does not wait for the delay';
    is @b.elems, 0, 'nothing is delivered before the delay';
    sleep 0.6;
    is-deeply @b, [1, 2, 3], 'the values arrive in order after the delay';
}

{
    my $d = Supply.from-list(1, 2, 3).delayed(0.1);
    my (@a, @b);
    my $pa = Promise.new;
    my $pb = Promise.new;
    $d.tap({ @a.push: $_ }, done => { $pa.keep(@a.elems) });
    $d.tap({ @b.push: $_ }, done => { $pb.keep(@b.elems) });
    is-deeply (await($pa), await($pb)), (3, 3), 'each tap gets every value, then done';
}

{
    my $done = Promise.new;
    Supply.from-list().delayed(0.1).tap({ flunk 'no value' }, done => { $done.keep(True) });
    ok await($done), 'an empty source is done after the delay stage';
}

is-deeply Supply.from-list(4, 5).delayed(0.05).list, (4, 5), '.list of a delayed finite supply';

{
    my @got;
    react { whenever Supply.from-list(7, 8).delayed(0.05) { @got.push: $_ } }
    is-deeply @got, [7, 8], 'a react block over a delayed finite supply';
}
