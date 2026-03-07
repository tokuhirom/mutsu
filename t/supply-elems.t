use Test;

plan 4;

{
    my @values;
    Supply.from-list(42..51).elems.tap(-> $v { @values.push($v) });
    is-deeply @values, [1..10], "Supply.elems traces cumulative counts for from-list";
}

{
    my $supplier = Supplier.new;
    my @values;
    my $done = False;
    $supplier.Supply.elems(1).tap(
      -> $v { @values.push($v) },
      :done({ $done = True }),
    );

    sleep 0.1;
    $supplier.emit(42);
    sleep 1;
    $supplier.emit(43);
    $supplier.emit(44);
    $supplier.emit(45);
    sleep 1;
    $supplier.emit(46);
    $supplier.done;

    for ^50 {
        last if $done;
        sleep 0.1;
    }

    ok $done, "Supplier-backed elems tap signals done";
    is-deeply @values, [2,5], "Supplier-backed elems(1) emits throttled cumulative counts";
}

ok (Supply.from-list(1..3).elems ~~ Supply), "Supply.elems returns a Supply";
