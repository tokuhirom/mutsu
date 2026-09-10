use Test;

plan 5;

{
    my @seen;
    my $tap = Supply.from-list(1..4).act(-> $v { @seen.push($v) });
    isa-ok $tap, Tap, "Supply.act returns Tap";
    is-deeply @seen, [1..4], "Supply.act replays source values";
}

{
    my $supplier = Supplier.new;
    my $done = False;
    $supplier.Supply.act: -> $ { }, done => { $done = True };

    $supplier.emit(42);
    nok $done, "done callback waits for supplier.done";

    $supplier.done;
    ok $done, "done callback fires on supplier.done";
}

my @nums = 1, 2, 3;
ok @nums == 3, "Array numeric comparison uses element count";
