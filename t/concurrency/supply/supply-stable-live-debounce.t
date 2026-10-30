use Test;

# Supply.stable($seconds) on a live supply: a value is emitted once `$seconds`
# pass without a newer one; `done` flushes the pending value first. cro's
# `Service.metadata-changed` (`$file-changed.grep(...).stable(0.5)`) relies on it.

plan 5;

{
    my $sup = Supplier.new;
    my @got;
    $sup.Supply.stable(0.3).tap: -> $v { @got.push($v) };
    sleep 0.1;
    $sup.emit(1);
    $sup.emit(2);
    sleep 1;
    $sup.emit(3);
    sleep 1;
    is-deeply @got, [2, 3], 'only the last value of a burst is emitted, after the quiet period';
}

{
    my $sup = Supplier.new;
    my @got;
    my $done = False;
    $sup.Supply.stable(0.3).tap: -> $v { @got.push($v) }, done => { $done = True };
    sleep 0.1;
    $sup.emit(1);
    $sup.emit(2);
    $sup.done;
    sleep 0.8;
    is-deeply @got, [2], 'done flushes the pending value';
    ok $done, 'and is passed on';
}

{
    my $sup = Supplier.new;
    my @got;
    my $s = supply { whenever $sup.Supply.stable(0.2) { emit $_ } };
    $s.tap: -> $v { @got.push($v) };
    sleep 0.1;
    $sup.emit('a');
    sleep 0.6;
    $sup.emit('b');
    $sup.emit('c');
    sleep 0.6;
    is-deeply @got, ['a', 'c'], 'inside a whenever';
}

{
    my $sup = Supplier.new;
    my @got;
    $sup.Supply.stable(0).tap: -> $v { @got.push($v) };
    sleep 0.1;
    $sup.emit(1);
    $sup.emit(2);
    sleep 0.2;
    is-deeply @got, [1, 2], '.stable(0) passes everything through';
}
