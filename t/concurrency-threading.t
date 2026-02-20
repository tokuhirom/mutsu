use Test;
plan 10;

# Test 1: start actually runs concurrently (two blocks with sleep)
{
    my $t0 = now;
    my $p1 = start { sleep 0.1; 1 };
    my $p2 = start { sleep 0.1; 2 };
    my @results = await $p1, $p2;
    my $elapsed = now - $t0;
    ok $elapsed < 0.5, "two start blocks run concurrently (elapsed: {$elapsed})";
    is @results[0], 1, 'first promise result';
    is @results[1], 2, 'second promise result';
}

# Test 2: await blocks until result
{
    my $p = start { sleep 0.05; 42 };
    is await($p), 42, 'await blocks and returns result';
}

# Test 3: Promise.new + keep/result
{
    my $p = Promise.new;
    is $p.status, 'Planned', 'new promise is Planned';
    $p.keep(99);
    is $p.status, 'Kept', 'promise becomes Kept';
    is $p.result, 99, 'result returns kept value';
}

# Test 4: Channel send/receive across threads
{
    my $c = Channel.new;
    start { sleep 0.05; $c.send(42) };
    my $val = $c.receive;
    is $val, 42, 'channel receive gets value from another thread';
}

# Test 5: Proc::Async captures real stdout
{
    my $proc = Proc::Async.new("echo", "hello-from-proc");
    my $promise = $proc.start;
    is await($promise), 0, 'proc::async returns exit code 0';
}

# Test 6: Promise.then chains
{
    my $p = start { 10 };
    my $p2 = $p.then(-> $v { $v * 2 });
    is await($p2), 20, 'promise.then chains correctly';
}
