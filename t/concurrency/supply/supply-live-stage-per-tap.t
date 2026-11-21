use Test;

# A live `map`/`grep`/`do`/`head` stage is a recipe instantiated per tap
# (ADR-18443, #18443): nothing runs until it is tapped, each tap runs the
# stage for itself, and closing the tap or the chain completing releases the
# source. `share` is the one point where taps share an instance. Red's live
# ResultSeq supply counts row materializations through these semantics
# (t/190-resultseq-supply).

plan 12;

my $n = 0;

{
    my $s = Supplier.new;
    my $m = $s.Supply.map({ $n++; $_ });
    $s.emit(1);
    is $n, 0, 'an untapped map does not run';
}

$n = 0;
{
    my $s = Supplier.new;
    my $m = $s.Supply.map({ $n++; $_ });
    my @a; my @b;
    $m.tap({ @a.push: $_ });
    $m.tap({ @b.push: $_ });
    $s.emit(5);
    is $n, 2, 'each tap runs the map for itself';
    is-deeply (@a, @b), ([5], [5]), 'and both receive the value';
}

$n = 0;
{
    my $s = Supplier.new;
    my $t = $s.Supply.grep({ $n++; True }).tap(-> $ { });
    $s.emit(1);
    $t.close;
    $s.emit(2);
    is $n, 1, 'a closed tap stops its grep';
}

$n = 0;
{
    my $s = Supplier.new;
    my @got;
    my $done = False;
    $s.Supply.map({ $n++; $_ * 10 }).head(1).tap({ @got.push: $_ }, done => { $done = True });
    $s.emit(1);
    $s.emit(2);
    is-deeply @got, [10], 'head passes its count on';
    ok $done, 'and is done';
    is $n, 1, 'a completed head releases the map below it';
}

$n = 0;
{
    my $s = Supplier.new;
    my $shared = $s.Supply.map({ $n++; $_ }).share;
    $shared.tap(-> $ { });
    $shared.tap(-> $ { });
    $s.emit(1);
    is $n, 1, 'share runs the stage once for all its taps';
}

{
    my $s = Supplier.new;
    ok $s.Supply.map(* + 1).live, 'map over a live source is live';
    nok $s.Supply.map(* + 1).head(2).live, 'head over it is not';
}

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(* * 2).grep(* > 2).do({ @got.push: "do:$_" }).tap({ @got.push: $_ });
    $s.emit($_) for 1..3;
    is-deeply @got, ['do:4', 4, 'do:6', 6], 'stacked stages run in order';
}

{
    # A combinator that is not a per-tap stage yet still sees a live source.
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(* * 10).batch(:2elems).tap({ @got.push: $_ });
    $s.emit($_) for 1..4;
    is-deeply @got, [(10, 20), (30, 40)], 'batch over a live map';
}
