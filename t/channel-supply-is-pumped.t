use Test;

plan 4;

# rakudo's `Channel.Supply` is PUMPED: a value sits on the channel until the
# scheduler moves it, so nothing counts as emitted to a tap at `send` time, and
# a value sent before anything tapped the channel is still there for the first
# tap to pick up.

# 1. A backlog sent before the tap existed is delivered.
{
    my $c = Channel.new;
    $c.send(1);
    $c.send(2);
    my @got;
    react {
        whenever $c.Supply -> $x { @got.push($x) }
        whenever Promise.in(0.3) { done }
    }
    is-deeply @got.List, (1, 2), 'values sent before the tap are still delivered';
}

# 2. A tap closed in the same react-body round, before the pump has run, gets
#    nothing -- the sends had not been emitted yet.
{
    my $c = Channel.new;
    my @got;
    react {
        my $t = do whenever $c.Supply -> $x { @got.push($x) };
        $c.send(1);
        $c.send(2);
        $t.close;
        whenever Promise.in(0.3) { done }
    }
    is-deeply @got.List, (), 'a close before the pump drops the sent values';
}

# 3. The same close moved into a later pump round delivers both -- the close
#    ordering rule itself is unchanged.
{
    my $c = Channel.new;
    my @got;
    react {
        my $t = do whenever $c.Supply -> $x { @got.push($x) };
        $c.send(1);
        $c.send(2);
        whenever Promise.in(0.1) { $t.close }
        whenever Promise.in(0.4) { done }
    }
    is-deeply @got.List, (1, 2), 'a close in a later round delivers both';
}

# 4. Closing the channel completes the subscription.
{
    my $c = Channel.new;
    my @got;
    start { $c.send($_) for 1..3; $c.close }
    react {
        whenever $c.Supply -> $x { @got.push($x) }
        whenever Promise.in(2) { done }
    }
    is-deeply @got.List, (1, 2, 3), 'a closed channel drains and completes';
}
