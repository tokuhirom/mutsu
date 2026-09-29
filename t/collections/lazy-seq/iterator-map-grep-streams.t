use Test;

# `.iterator` over a deferred `.map` / `.grep` Seq streams its source: each
# protocol call runs the callback over only the elements it hands out, as
# Rakudo's map iterator does (#10186).

plan 31;

{
    my @log;
    my $it = (1..5).map({ @log.push($_); $_ * 10 }).iterator;
    is @log.elems, 0, '.iterator runs no callback';
    is $it.pull-one, 10, 'pull-one yields the first mapped element';
    is @log, [1], '... after running the callback once';
    my @target;
    is $it.push-exactly(@target, 2), 2, 'push-exactly returns the count';
    is @target, [20, 30], '... and pushes the next two elements';
    is @log, [1, 2, 3], '... running the callback twice more';
    is $it.skip-one, 1, 'skip-one skips';
    is $it.pull-one, 50, 'pull-one after skip-one';
    is @log, [1, 2, 3, 4, 5], 'every callback ran exactly once';
    ok $it.pull-one =:= IterationEnd, 'exhausted iterator returns IterationEnd';
    ok $it.pull-one =:= IterationEnd, '... and keeps returning it';
}

{
    my $c = 0;
    my $it = (1..10).grep({ $c++; $_ %% 3 }).iterator;
    is $it.pull-one, 3, 'grep iterator yields the first match';
    is $c, 3, '... testing only up to it';
    is $it.pull-one, 6, 'second match';
    is $c, 6, '... resuming where it stopped';
}

{
    my @big = ^200_000;
    my $c = 0;
    is @big.map({ $c++; $_ + 1 }).iterator.pull-one, 1,
        'pull-one over a large source';
    is $c, 1, '... runs one callback regardless of the source size';
}

{
    my $it = (1..4).map(* * 2).iterator;
    $it.pull-one;
    my @rest;
    ok $it.push-all(@rest) =:= IterationEnd, 'push-all returns IterationEnd';
    is @rest, [4, 6, 8], '... after pushing the remaining elements';
}

{
    my $c = 0;
    my $it = (1..6).map({ $c++; $_ }).iterator;
    is Seq.new($it).head(2), (1, 2), 'Seq.new over a stream iterator';
    is $c, 2, '... pulls only what .head needs';
}

{
    my @its = (1..3).map(* + 10).iterator, (1..2).map(* + 20).iterator;
    is (@its[0].pull-one, @its[1].pull-one, @its[0].pull-one), (11, 21, 12),
        'stream iterators held in an array advance independently';
}

{
    my $it = (1..5).map({ last if $_ == 3; $_ }).iterator;
    is ($it.pull-one, $it.pull-one), (1, 2), 'elements before a last';
    ok $it.pull-one =:= IterationEnd, 'last in the callback ends the iterator';
}

{
    my $it = (1..2).map({ slip($_, $_ * 10) }).iterator;
    is ($it.pull-one, $it.pull-one, $it.pull-one, $it.pull-one), (1, 10, 2, 20),
        'a Slip from the callback is handed out one element at a time';
    ok $it.pull-one =:= IterationEnd, '... then IterationEnd';
}

{
    my $s = (1..5).map(* + 1);
    ok ?$s, 'boolification pulls a prefix';
    my $it = $s.iterator;
    is ($it.pull-one, $it.pull-one), (2, 3), 'an iterator after a prefix pull starts at the front';
}

{
    my $n = 0;
    my $it = (^1000).map(* * 2).iterator;
    until $it.pull-one =:= IterationEnd { $n++ }
    is $n, 1000, 'draining with pull-one yields every element';
}

{
    my $it = (1..4).map(* * 2).iterator;
    $it.pull-one;
    is List.from-iterator($it), (4, 6, 8), 'List.from-iterator takes the rest of the stream';
    ok $it.pull-one =:= IterationEnd, '... leaving the iterator exhausted';
}
