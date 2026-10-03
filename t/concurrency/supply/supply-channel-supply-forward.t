use Test;

# `$supplier.Supply.Channel.Supply`: a value emitted on the supplier after the
# chain is tapped must reach the tap. The `Supply.Channel` forwarding tap only
# queued the value on the channel, bypassing `Channel.send`'s delivery to the
# channel's own Supplies, so the tap never fired. Reduced from
# Cro::WebSocket's Handler, which feeds the user block from
# `$supplier.Supply.Channel.Supply.grep(...)`.

plan 4;

{
    my $supplier = Supplier.new;
    my $feed = $supplier.Supply.Channel.Supply;
    my $got = Promise.new;
    $feed.tap: -> $v { $got.keep($v) };
    $supplier.emit('hi');
    await Promise.anyof($got, Promise.in(5));
    is $got.result, 'hi', 'emit after tap reaches Channel.Supply';
}

{
    my $supplier = Supplier.new;
    my $feed = $supplier.Supply.Channel.Supply.grep(* ne 'skip');
    my $res = supply { whenever $feed -> $m { emit $m.uc } };
    my $got = Promise.new;
    $res.tap: -> $v { $got.keep($v) };
    $supplier.emit('skip');
    $supplier.emit('hi');
    await Promise.anyof($got, Promise.in(5));
    is $got.result, 'HI', 'through grep and a supply block';
}

{
    my $supplier = Supplier.new;
    my $ch = $supplier.Supply.Channel;
    $supplier.emit('queued');
    is $ch.poll, 'queued', 'a channel with no Supply still queues';
}

{
    my $c = Channel.new;
    my @seen;
    $c.Supply.tap: -> $v { @seen.push($v) };
    $c.send(1);
    $c.send(2);
    is-deeply @seen, [1, 2], 'plain Channel.send to Channel.Supply unchanged';
}
