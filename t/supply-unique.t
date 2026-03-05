use Test;
plan 8;

# Basic unique on a from-list supply
{
    my $s = Supply.from-list(1, 2, 2, 3, 3, 3, 4);
    my @res;
    my $done;
    $s.unique.tap(-> $v { @res.push($v) }, :done({ $done = True }));
    for ^100 { last if $done; sleep .01 }
    is-deeply @res, [1, 2, 3, 4], "unique removes duplicates from from-list supply";
}

# Unique with :as
{
    my $s = Supply.from-list("a", "A", "b", "B", "c");
    my @res;
    my $done;
    $s.unique(:as(&lc)).tap(-> $v { @res.push($v) }, :done({ $done = True }));
    for ^100 { last if $done; sleep .01 }
    is-deeply @res, ["a", "b", "c"], "unique with :as uses transform for comparison";
}

# Unique with :with
{
    my $s = Supply.from-list(1, 2, 2, 3, 3, 3, 4);
    my @res;
    my $done;
    $s.unique(:with(&[==])).tap(-> $v { @res.push($v) }, :done({ $done = True }));
    for ^100 { last if $done; sleep .01 }
    is-deeply @res, [1, 2, 3, 4], "unique with :with uses custom comparator";
}

# Unique on a supplier-backed supply (non-expires)
{
    my $s = Supplier.new;
    my $supply = $s.Supply.unique;
    my @res;
    my $done;
    $supply.tap(-> $v { @res.push($v) }, :done({ $done = True }));
    $s.emit(1);
    $s.emit(2);
    $s.emit(2);
    $s.emit(3);
    $s.emit(1);
    $s.done;
    for ^100 { last if $done; sleep .01 }
    is-deeply @res, [1, 2, 3], "unique on supplier-backed supply filters duplicates";
}

# Unique on supplier-backed supply with :as
{
    my $s = Supplier.new;
    my $supply = $s.Supply.unique(:as(&lc));
    my @res;
    my $done;
    $supply.tap(-> $v { @res.push($v) }, :done({ $done = True }));
    $s.emit("a");
    $s.emit("A");
    $s.emit("b");
    $s.emit("B");
    $s.done;
    for ^100 { last if $done; sleep .01 }
    is-deeply @res, ["a", "b"], "unique on supplier-backed supply with :as";
}

# Unique with :expires on supplier-backed supply
{
    my $s = Supplier.new;
    my $supply = $s.Supply.unique(:expires(1));
    my @res;
    my $done;
    $supply.tap(-> $v { @res.push($v) }, :done({ $done = True }));
    $s.emit(1);
    $s.emit(1);   # should be filtered
    sleep 1.5;
    $s.emit(1);   # should pass (expired)
    $s.done;
    for ^100 { last if $done; sleep .01 }
    is-deeply @res, [1, 1], "unique with :expires allows repeated values after expiry";
}

# Unique preserves order
{
    my $s = Supply.from-list(5, 3, 5, 1, 3, 2);
    my @res;
    my $done;
    $s.unique.tap(-> $v { @res.push($v) }, :done({ $done = True }));
    for ^100 { last if $done; sleep .01 }
    is-deeply @res, [5, 3, 1, 2], "unique preserves first-seen order";
}

# Unique returns a Supply
{
    my $s = Supplier.new;
    ok $s.Supply.unique ~~ Supply, "unique returns a Supply";
}
