use Test;

# `my $tap = $supply.tap({ ...; $tap.close })` on a channel-backed source: the
# callback runs on a worker thread, and the first event may already be waiting
# when `.tap` is called. The consumer is held until the declaration has stored
# the Tap, so the callback can never observe `$tap` unbound -- however long the
# initializer takes after `.tap` returns (#9590).

plan 4;

# Burn CPU without blocking: a blocking call would (deliberately) give up the
# ordering, see the last test.
sub spin($secs) { my $end = now + $secs; my $n = 0; $n++ while now < $end; $n }

# Hands `$value` back only after spinning: the declaration it initializes
# stores it that much later than `.tap` returned.
sub late($value, $secs) { spin($secs); $value }

{
    my $seen = Promise.new;
    my $v = $seen.vow;
    my $tap = late(Supply.interval(0.001).tap({
        $v.keep($tap.^name) if $seen.status ~~ Planned;
        $tap.close;
    }), 0.2);
    await Promise.anyof($seen, Promise.in(10));
    is $seen.result, 'Tap', 'an interval tap callback waits for its declaration to store the Tap';
}

{
    my @names;
    for ^5 {
        my $seen = Promise.new;
        my $v = $seen.vow;
        my $tap = late(Supply.interval(0.001).tap({
            $v.keep($tap.^name) if $seen.status ~~ Planned;
            $tap.close;
        }), 0.02);
        await Promise.anyof($seen, Promise.in(10));
        @names.push: $seen.result;
    }
    is @names, <Tap Tap Tap Tap Tap>, '... on every iteration of a loop redeclaring it';
}

{
    my $seen = Promise.new;
    my $v = $seen.vow;
    my $tap = late(supply { whenever Supply.interval(0.001) { emit $_ } }.tap({
        $v.keep($tap.^name) if $seen.status ~~ Planned;
    }), 0.1);
    await Promise.anyof($seen, Promise.in(10));
    $tap.close;
    is $seen.result, 'Tap', 'a tap on a supply block with a live whenever source sees its declaration';
}

# The initializer itself waits for the callback: holding the callback until the
# store would deadlock, so blocking releases it and the callback sees the
# declaration still unbound, as in Rakudo.
{
    my $seen = Promise.new;
    my $v = $seen.vow;
    sub wait-for($value) { await Promise.anyof($seen, Promise.in(10)); $value }
    my $tap = wait-for(Supply.interval(0.01).tap({ $v.keep($tap.^name) if $seen.status ~~ Planned }));
    $tap.close;
    is $seen.status ~~ Kept ?? $seen.result !! 'hung', 'Any',
        'an initializer awaiting its own tap callback does not deadlock';
}
