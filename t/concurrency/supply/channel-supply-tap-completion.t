use Test;

plan 5;

# Closing a `Channel` completes the taps on its `Supply`. `Channel.close` used
# to mark each bridged supplier done -- which raises the terminal flag and wakes
# the sinks -- but never drained the `done => { ... }` callbacks those taps had
# registered, so every value arrived and the completion signal never did.
# `Channel.fail` did not even reach the supplier, so `quit => { ... }` never ran.
#
# Every assertion below observes a `Promise` the handler keeps rather than
# printed output, so it does not depend on the ordering of writes across
# threads.

# 1. `.close` runs the tap's `done` callback.
{
    my $c = Channel.new;
    my $p = Promise.new;
    $c.Supply.tap(-> $v { }, done => { $p.keep });
    $c.send(1);
    $c.close;
    is await(Promise.anyof($p, Promise.in(5))) && $p.status, 'Kept',
        'Channel.close fires the tap done callback';
}

# 2. `.fail` runs the tap's `quit` callback -- the same completion edge.
{
    my $c = Channel.new;
    my $p = Promise.new;
    $c.Supply.tap(-> $v { }, quit => -> $ex { $p.keep });
    $c.send(1);
    $c.fail("boom");
    is await(Promise.anyof($p, Promise.in(5))) && $p.status, 'Kept',
        'Channel.fail fires the tap quit callback';
}

# 3. done fires exactly once per tap when one Supply carries several taps.
{
    my $c = Channel.new;
    my $n = 0;
    my $s = $c.Supply;
    $s.tap(-> $v { }, done => { $n++ });
    $s.tap(-> $v { }, done => { $n++ });
    $c.send(1);
    $c.close;
    sleep 1;
    is $n, 2, 'each tap on one Channel.Supply is completed exactly once';
}

# 4. ... and when each tap comes from its own `.Supply` call (a distinct
#    supplier bridged onto the same channel).
{
    my $c = Channel.new;
    my $n = 0;
    $c.Supply.tap(-> $v { }, done => { $n++ });
    $c.Supply.tap(-> $v { }, done => { $n++ });
    $c.send(1);
    $c.close;
    sleep 1;
    is $n, 2, 'each Channel.Supply view is completed exactly once';
}

# 5. Completion comes after the values, not instead of them.
{
    my $c = Channel.new;
    my @got;
    $c.Supply.tap(-> $v { @got.push($v) }, done => { @got.push('DONE') });
    $c.send(1);
    $c.send(2);
    $c.close;
    sleep 1;
    is @got.tail, 'DONE', 'done runs after the last value, not before it';
}
