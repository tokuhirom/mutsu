use Test;

plan 3;

# Multiple `whenever` blocks in one `react` share the enclosing block's
# lexicals. A captured-outer variable mutated by more than one whenever must
# accumulate every sibling's write; the per-closure-instance captured state
# must not restore a stale snapshot that clobbers a sibling's update.

# Two channels feeding two whenevers that both append to `$order`, coordinated
# via Promises so the writes strictly interleave c1 -> c2 -> c1.
{
    my $c1 = Channel.new;
    my $c2 = Channel.new;
    my $p1 = Promise.new;
    my $p2 = Promise.new;
    start { $c1.send(1); await $p2; $c1.send(3); $c1.close(); }
    start { await $p1; $c2.send(2); $c2.close(); }
    my $order = '';
    react {
        whenever $c1 { $order ~= $_; $p1.keep(True) unless $p1; }
        whenever $c2 { $order ~= $_; $p2.keep(True); }
    }
    is $order, '123', 'sibling whenevers accumulate writes to a shared lexical';
}

# A single whenever re-entered many times still accumulates into a shared
# captured-outer counter (per-instance state must reflect the live value).
{
    my $c = Channel.new;
    start { $c.send($_) for 1..5; $c.close(); }
    my $sum = 0;
    react {
        whenever $c { $sum += $_; }
    }
    is $sum, 15, 're-entered whenever accumulates into a captured-outer counter';
}

# A captured-outer array shared across two whenevers collects from both.
{
    my $c1 = Channel.new;
    my $c2 = Channel.new;
    start { $c1.send('a'); $c2.send('b'); $c1.close(); $c2.close(); }
    my @seen;
    react {
        whenever $c1 { @seen.push: $_; }
        whenever $c2 { @seen.push: $_; }
    }
    is @seen.sort.join, 'ab', 'both whenevers push into a shared captured-outer array';
}
