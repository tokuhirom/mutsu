use Test;

# `stable` over a live source, and `migrate` over a live recipe, build their
# stage once per tap (ADR-18443, #18579): each tap runs its own instance of
# the chain below, and a closed tap gets nothing more.

plan 5;

{
    my $n = 0;
    my $s = Supplier.new;
    my $st = $s.Supply.map({ $n++; $_ }).stable(0.05);
    my (@a, @b);
    $st.tap({ @a.push: $_ });
    $st.tap({ @b.push: $_ });
    $s.emit(1);
    $s.emit(2);
    sleep 0.4;
    is "@a[] | @b[]", '2 | 2', 'each tap of a stable recipe sees the settled value';
    is $n, 4, 'each tap runs its own map';
}

{
    my $s = Supplier.new;
    my $st = $s.Supply.stable(0.05);
    my (@a, @b);
    my $ta = $st.tap({ @a.push: $_ });
    $st.tap({ @b.push: $_ });
    $s.emit(1);
    sleep 0.3;
    $ta.close;
    $s.emit(3);
    sleep 0.3;
    is "@a[] | @b[]", '1 | 1 3', 'a closed tap of a live stable gets nothing more';
}

{
    my $s = Supplier.new;
    my @c;
    my $done = False;
    $s.Supply.map(* * 10).stable(10).tap({ @c.push: $_ }, done => { $done = True });
    $s.emit(1);
    $s.done;
    sleep 0.3;
    ok $done, 'done passes through a per-tap stable';
}

{
    my $k = 0;
    my $outer = Supplier.new;
    my $mi = $outer.Supply.map({ $k++; $_ }).migrate;
    my @e;
    $mi.tap({ @e.push: $_ });
    my $inner = Supplier.new;
    $outer.emit($inner.Supply);
    $inner.emit(42);
    is "@e[] $k", '42 1', 'migrate over a recipe';
}
