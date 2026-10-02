use Test;

# The `.on-close` callback of a live `whenever` source runs when the supply
# block stops listening to it: when the block's tap is closed, and when the
# block ends normally (issue #10833).

plan 6;

{
    my $s = Supplier.new;
    my $closed = Promise.new;
    my $src = $s.Supply.on-close({ $closed.keep(True) unless $closed });
    my $t = supply { whenever $src { emit $_ } }.tap;
    is $closed.status, Planned, 'on-close has not run while the tap is open';
    $t.close;
    is $closed.status, Kept, 'closing the block tap runs the source on-close';
}

{
    my $s = Supplier.new;
    my $count = 0;
    my $done = False;
    my $src = $s.Supply.on-close({ $count++ });
    supply { whenever $src { emit $_ } }.tap(done => { $done = True });
    $s.done;
    ok $done, 'the block completes when its source does';
    ok $count >= 1, 'completion runs the source on-close';
}

{
    # Stomp::Server.listen's shape: a listener supply whose on-close marks it
    # closed, consumed by a whenever.
    my $connections = Supplier.new;
    my $is-closed = Promise.new;
    my $listener = $connections.Supply.on-close({ $is-closed.keep(True) unless $is-closed });
    my @got;
    my $tap = supply { whenever $listener -> $conn { emit $conn } }.tap({ @got.push($_) });
    $connections.emit('c1');
    $tap.close;
    is-deeply @got, ['c1'], 'values flowed before the close';
    ok (await Promise.anyof($is-closed, Promise.in(5))) && $is-closed.status ~~ Kept,
        'closing the tap closes the listener';
}
