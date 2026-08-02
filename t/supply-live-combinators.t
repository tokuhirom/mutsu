use Test;

# Combinators over a *live* (Supplier-backed) Supply must stay connected to
# their source. A combinator's Supply is tapped before anything is emitted, so
# any implementation that snapshots the source's values at combinator time
# produces an empty Supply that fires `done` immediately and silently drops
# every later emission.

plan 13;

# `live` is a real method (`method live(Supply:D: --> Bool:D)`), not an
# attribute accessor: every Supply answers it, whatever built it.
{
    my $s1 = Supplier.new;
    my $s2 = Supplier.new;
    ok $s1.Supply.live, 'a Supplier-backed Supply is live';
    nok Supply.from-list(1, 2, 3).live, 'an on-demand Supply is not live';
    nok $s1.Supply.merge($s2.Supply).live, 'a merged Supply is not live';
    nok Supply.from-list(1, 2).rotor(2).live, 'a rotored Supply is not live';
}

# merge: values from every source interleave, and the merged Supply is done
# only once *every* source is done -- not when the first one finishes.
{
    my $s1 = Supplier.new;
    my $s2 = Supplier.new;
    my @res;
    my $done = 0;
    $s1.Supply.merge($s2.Supply).tap({ @res.push($_) }, :done({ $done++ }));
    $s1.emit(1);
    $s2.emit('a');
    $s1.emit(2);
    $s1.done;
    is $done, 0, 'a merge is not done while a source is still live';
    $s2.emit('b');
    $s2.done;
    is-deeply @res, [1, 'a', 2, 'b'], 'a merge forwards every live source';
    is $done, 1, 'a merge is done once every source is';
}

# reduce: folds as the source emits, and delivers the single result at done.
{
    my $s = Supplier.new;
    my @res;
    my $done = 0;
    $s.Supply.reduce({ $^a + $^b }).tap({ @res.push($_) }, :done({ $done++ }));
    $s.emit($_) for 1, 2, 3, 4;
    is-deeply @res, [], 'a live reduce emits nothing before done';
    $s.done;
    is-deeply @res, [10], 'a live reduce emits its fold at done';
    is $done, 1, 'a live reduce is done with its source';
}

# classify: each group is preserving. The outer tap only collects the Pairs, so
# the group Supply is invariably tapped after values have already landed in it.
{
    my $s = Supplier.new;
    my @keys;
    my @groups;
    $s.Supply.classify({ $_ %% 2 }).tap(-> $p { @keys.push($p.key); @groups.push($p.value) });
    $s.emit($_) for 1, 2, 3, 4;
    $s.done;
    is-deeply @keys, [False, True], 'classify emits one Pair per group';
    my @first;
    @groups[0].tap({ @first.push($_) });
    is-deeply @first, [1, 3], 'a group tapped after the fact replays its values';
}

# `Supply.rotor` emits each group as an Array, not the List that `List.rotor`
# produces -- rakudo's Supply combinator collects into an `@batched` array.
{
    my @res;
    Supply.from-list(1 .. 5).rotor(3 => -2).tap({ @res.push($_) });
    is-deeply @res, [[1, 2, 3], [2, 3, 4], [3, 4, 5]], 'Supply.rotor emits Arrays';
}

# vim: expandtab shiftwidth=4
