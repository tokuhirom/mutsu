use Test;

# A live source's `done` reaches every stage of a combinator chain, so a
# stage that acts at done (reduce, batch, ...) still does when it is tapped
# on a DERIVED supply rather than on the source itself (#11647).

plan 7;

{
    my $s = Supplier.new;
    my @seen;
    $s.Supply.map(* + 0).reduce({ $^a + $^b }).tap({ @seen.push($_) });
    $s.emit(1); $s.emit(2); $s.done;
    is-deeply @seen, [3], 'reduce after a live map delivers its fold at done';
}

{
    my $s = Supplier.new;
    my @seen;
    $s.Supply.grep(* > 0).grep(* > 0).reduce({ $^a + $^b }).tap({ @seen.push($_) });
    $s.emit($_) for 1..3; $s.done;
    is-deeply @seen, [6], 'reduce after two live greps';
}

{
    my $s = Supplier.new;
    my @seen;
    my $done = False;
    $s.Supply.map(* * 2).batch(elems => 5).tap({ @seen.push($_) }, done => { $done = True });
    $s.emit(1); $s.emit(2); $s.done;
    is-deeply @seen, [(2, 4),], 'batch after a live map flushes its partial batch';
    ok $done, '... and is done';
}

{
    my $s = Supplier.new;
    my @seen;
    $s.Supply.head(5).reduce({ $^a + $^b }).tap({ @seen.push($_) });
    $s.emit(1); $s.emit(2); $s.done;
    is-deeply @seen, [3], 'reduce after a live head that did not reach its limit';
}

{
    my $s = Supplier.new;
    my @seen;
    $s.Supply.head(2).reduce({ $^a + $^b }).tap({ @seen.push($_) });
    $s.emit($_) for 1..4;
    is-deeply @seen, [3], 'reduce after a live head that reached its limit, no source done';
}

{
    my $s = Supplier.new;
    my $done = 0;
    $s.Supply.map(* + 0).map(* + 0).tap({;}, done => { $done++ });
    $s.done;
    is $done, 1, 'a two-stage chain fires its done exactly once';
}
