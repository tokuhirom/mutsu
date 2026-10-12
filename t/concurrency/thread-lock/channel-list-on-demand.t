use Test;

# `Channel.list` / `.Seq` receive one item per element, on demand (#12727):
# a `for $c.list` consumer runs its body as each item arrives, not only once
# the channel closes.

plan 14;

# A request/response handshake over two channels: the worker must answer
# request 1 before the main thread sends request 2, which deadlocks if the
# loop waits for the next item (or the close) before running its body.
{
    my $req  = Channel.new;
    my $resp = Channel.new;
    my $worker = start { for $req.list -> $n { $resp.send($n * 10) } };
    my @got;
    for 1..3 -> $n {
        $req.send($n);
        my $r = start { $resp.receive };
        await Promise.anyof($r, Promise.in(10));
        @got.push: $r.status ~~ Kept ?? $r.result !! 'timeout';
    }
    $req.close;
    await $worker;
    is-deeply @got, [10, 20, 30], 'for $c.list answers each item before the next send';
}

# The same through `.Seq`.
{
    my $req  = Channel.new;
    my $resp = Channel.new;
    my $worker = start { for $req.Seq -> $n { $resp.send($n + 1) } };
    $req.send(1);
    my $r = start { $resp.receive };
    await Promise.anyof($r, Promise.in(10));
    is $r.status ~~ Kept ?? $r.result !! 'timeout', 2,
        'for $c.Seq answers an item before the close';
    $req.close;
    await $worker;
}

{
    my $c = Channel.new;
    $c.send($_) for 1..3;
    $c.close;
    my $l = $c.list;
    is $l.WHAT.^name, 'List', '.list is a List';
    is-deeply $l.List, (1, 2, 3), '.list holds every item sent before the close';
}

{
    my $c = Channel.new;
    $c.send(4);
    $c.close;
    my $s = $c.Seq;
    is $s.WHAT.^name, 'Seq', '.Seq is a Seq';
    is-deeply $s.List, (4,), '.Seq holds the items, not the Channel';
}

{
    my $c = Channel.new;
    $c.send($_) for 1..2;
    $c.close;
    my @a = $c.list;
    is-deeply @a, [1, 2], 'assigning .list to an array drains to the close';
}

{
    my $c = Channel.new;
    $c.send(5);
    $c.fail('boom');
    my @seen;
    try { for $c.list { @seen.push: $_ } }
    is-deeply @seen, [5], 'items before a failure are delivered';
    is $!.message, 'boom', 'the failure is rethrown from the loop';
}

# The eager forms, and consumers that drain the on-demand list.
sub filled { my $c = Channel.new; $c.send($_) for 1..3; $c.close; $c }
is-deeply filled.List, (1, 2, 3), '.List drains the items';
is-deeply filled.Array, [1, 2, 3], '.Array drains to a real Array';
is-deeply filled.Capture, \(1, 2, 3), '.Capture drains the items';
is-deeply filled.list.Capture, \(1, 2, 3), '.list.Capture pulls every item';
is-deeply (|filled.list, 9), (1, 2, 3, 9), 'slipping .list pulls every item';
