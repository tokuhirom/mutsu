use Test;

plan 3;

# `whenever $channel` inside a `supply { }` block coerces the Channel through
# `.Supply`, exactly like the `react` form does.
{
    my $ch = Channel.new;
    my @got;
    my $s = supply { whenever $ch -> $v { @got.push: $v; emit $v } };
    my $tap = $s.tap(-> $ { });
    $ch.send(1);
    $ch.send(2);
    my $deadline = now + 5;
    sleep 0.05 until @got == 2 || now > $deadline;
    is-deeply @got, [1, 2], 'values sent after the tap reach a bare-channel whenever';
    $tap.close;
}

{
    my $ch = Channel.new;
    $ch.send('early');
    my @got;
    my $tap = (supply { whenever $ch -> $v { @got.push: $v; emit $v } }).tap(-> $ { });
    my $deadline = now + 5;
    sleep 0.05 until @got == 1 || now > $deadline;
    is-deeply @got, ['early'], 'a value queued before the tap is delivered';
    $tap.close;
}

{
    my $ch = Channel.new;
    my @got;
    my $done = Promise.new;
    (supply { whenever $ch -> $v { emit $v } }).tap({ @got.push: $_ }, done => { $done.keep });
    $ch.send(7);
    $ch.close;
    await Promise.anyof($done, Promise.in(5));
    is-deeply @got, [7], 'bare-channel whenever delivers then completes on close';
}
