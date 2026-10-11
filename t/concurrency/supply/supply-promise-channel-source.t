use Test;

plan 3;

# `supply { whenever $channel.Supply { } }.Promise` drains the channel while it
# runs; values sent after the coercion are not lost (#12750).
{
    my $ch = Channel.new;
    my @got;
    my $p = supply { whenever $ch.Supply -> $v { @got.push: $v } }.Promise;
    $ch.send('a');
    $ch.send('b');
    $ch.close;
    await Promise.anyof($p, Promise.in(5));
    is $p.status, Kept, 'promise is kept once the channel is closed';
    is-deeply @got, ['a', 'b'], 'values sent after .Promise are delivered';
}

# A bare channel source behaves the same.
{
    my $ch = Channel.new;
    my @got;
    my $p = supply {
        whenever $ch -> $v { @got.push: $v }
        whenever Promise.in(0.5) { done }
    }.Promise;
    $ch.send(1);
    await Promise.anyof($p, Promise.in(5));
    is-deeply @got, [1], 'bare channel + timer: the value is delivered before done';
}
