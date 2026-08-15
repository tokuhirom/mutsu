use Test;
plan 3;

# todo/tickets/head-on-a-channel-backed-supply-drops-every-value.md: `.head(N)`
# on a channel-backed live Supply (Supply.interval without :scheduler, here)
# used to take the *materialized-values* branch (empty, since the Supply had
# not even been tapped yet), silently dropping every emission.
{
    my @res;
    my $done;
    Supply.interval(0.1).head(3).tap({ @res.push($_) }, :done({ $done = True }));
    for ^40 { last if $done; sleep .1 }
    is @res.elems, 3, 'head(3) on a channel-backed Supply delivers exactly 3 values';
    is-deeply @res, [0, 1, 2], 'and they are the first 3 emissions, in order';
}

# head(0) never taps the callback at all, but still fires done.
{
    my @res;
    my $done;
    Supply.interval(0.1).head(0).tap({ @res.push($_) }, :done({ $done = True }));
    for ^40 { last if $done; sleep .1 }
    is @res.elems, 0, 'head(0) on a channel-backed Supply never calls the tap body';
}
