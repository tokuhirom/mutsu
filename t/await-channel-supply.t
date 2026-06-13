use Test;

# `await` on a Channel receives the next value (blocking); `await` on a Supply
# runs it to completion and returns the last emitted value (broken if it quits).

plan 8;

# await on a Channel returns the next sent value.
{
    my $c = Channel.new;
    $c.send(42);
    is await($c), 42, 'await Channel receives the sent value';
}

# Two awaits drain two values in order.
{
    my $c = Channel.new;
    $c.send(1); $c.send(2);
    is (await($c) + await($c)), 3, 'sequential awaits drain the channel';
}

# await on a closed, drained Channel throws X::Channel::ReceiveOnClosed.
{
    my $c = Channel.new;
    $c.close;
    throws-like { await($c) }, X::Channel::ReceiveOnClosed,
        'await on a closed empty Channel throws';
}

# await on a supply block returns the last value emitted before done.
{
    is (await supply { emit 1; emit 2; emit 7; done }), 7,
        'await supply block returns the last emitted value';
}

# await on a supply driven by Supply.interval returns its emitted value.
{
    is (await supply { whenever Supply.interval(0.01) { emit 99; done } }), 99,
        'await supply with interval returns the emitted value';
}

# A die inside a whenever quits the supply; await rethrows the cause
# (instead of hanging until the poll deadline).
{
    throws-like { await supply { whenever Supply.interval(0.01) { die 'strewth' } } },
        Exception, message => 'strewth',
        'await of a dying supply rethrows the cause';
}

# Many outstanding awaits over supplies in parallel start blocks all complete.
{
    my @proms = (1..20).map: -> $i {
        start { await supply { whenever Supply.interval(0.001 * $i) { emit $i; done } } }
    };
    is ([+] await @proms), (1..20).sum, 'many outstanding supply awaits complete';
}

# Many outstanding awaits over a shared channel in parallel start blocks.
{
    my $c = Channel.new;
    my @proms = (1..20).map: { start { await($c) } };
    for 1..20 { $c.send($_) }
    is ([+] await @proms), (1..20).sum, 'many outstanding channel awaits complete';
}
