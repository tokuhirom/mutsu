use Test;

# ADR-18443: the combinators that emit only once their source is done, from
# everything it emitted (`sort`, `reverse`, `rotate`, `grab`, ...), are per-tap
# recipes over a live source (#18443): nothing runs until a tap, and each tap
# collects for itself. A plain live source used to be snapshotted when the
# method ran, so `$supplier.Supply.sort` emitted nothing.

plan 3;

{
    my $n = 0;
    my $s = Supplier.new;
    my $src = $s.Supply.map({ $n++; $_ });
    my @x = $src.sort, $src.reverse, $src.rotate(1), $src.grab({ $_.sum });
    $s.emit(1);
    is $n, 0, 'untapped collecting stages run nothing';
    my $r = $src.sort;
    my (@a, @b);
    $r.tap({ @a.push: $_ }, done => { @a.push: 'done' });
    $s.emit(3);
    $r.tap({ @b.push: $_ });
    $s.emit(2);
    $s.done;
    is-deeply ($n, @a, @b), (3, [2, 3, 'done'], [2]), 'each sort tap collects for itself';
}

{
    my $s = Supplier.new;
    my (@rv, @rt, @g, @p);
    $s.Supply.map(* * 1).reverse.tap({ @rv.push: $_ });
    $s.Supply.map(* * 1).rotate(1).tap({ @rt.push: $_ });
    $s.Supply.map(* * 1).grab({ .sum }).tap({ @g.push: $_ });
    $s.Supply.sort.tap({ @p.push: $_ });
    $s.emit($_) for 3, 1, 2;
    $s.done;
    is-deeply (@rv, @rt, @g, @p), ([2, 1, 3], [1, 2, 3], [6], [1, 2, 3]),
        'reverse, rotate, grab and sort over live sources';
}
