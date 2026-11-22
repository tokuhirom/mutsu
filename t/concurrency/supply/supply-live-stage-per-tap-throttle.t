use Test;

# `throttle` over a live source is a per-tap stage (ADR-18443, #18579). It
# used to read the source's (empty) materialized values and emit nothing.

plan 6;

{
    my $s = Supplier.new;
    my @a;
    my $done = False;
    $s.Supply.throttle(2, 1).tap({ @a.push: $_ }, done => { $done = True });
    $s.emit($_) for 1..5;
    sleep 0.3;
    is @a.join(' '), '1 2', 'the first window passes `limit` values';
    $s.done;
    sleep 2.5;
    is @a.join(' '), '1 2 3 4 5', 'the rest follow in later windows';
    ok $done, 'done waits for the queue';
}

{
    my $n = 0;
    my $s = Supplier.new;
    my $th = $s.Supply.map({ $n++; $_ }).throttle(10, 0.01);
    my (@a, @b);
    $th.tap({ @a.push: $_ });
    $th.tap({ @b.push: $_ });
    $s.emit($_) for 1..3;
    sleep 0.3;
    is "@a[] | @b[] | $n", '1 2 3 | 1 2 3 | 6', 'each tap of a throttle recipe runs its own chain';
}

{
    my $s = Supplier.new;
    my @p;
    $s.Supply.throttle(2, { $_ * 10 }).tap({ @p.push: $_ });
    $s.emit($_) for 1..3;
    sleep 0.5;
    ok @p.elems == 3 && @p.all ~~ Promise, 'the block form emits a Promise per value';
    is @p.map(*.result).sort.join(' '), '10 20 30', 'each kept with the block result';
}
