use Test;

plan 5;

{
    my $caught = "";
    my @seen;

    try {
        my $s = supply {
            emit 1;
            die "boom";
        };
        $s.tap(-> $v { @seen.push($v) });
        CATCH {
            default { $caught = .Str }
        }
    }

    is-deeply @seen, [1], 'tap receives values emitted before die';
    is $caught, 'boom', 'die inside supply propagates to tap caller';
}

{
    my $supplier = Supplier.new;
    my @seen;
    my $done = False;

    $supplier.Supply.tap(
        -> $v { @seen.push($v) },
        done => -> { $done = True },
    );

    $supplier.emit(1);
    $supplier.done;
    $supplier.emit(2);

    is-deeply @seen, [1], 'emit after Supplier.done is ignored';
    ok $done, 'Supplier.done triggers done callback';
}

{
    my @events;
    my $supplier = Supplier.new;

    start {
        sleep 0.05;
        $supplier.emit(1);
        $supplier.quit("boom");
    };

    react {
        whenever $supplier.Supply {
            @events.push("value($_)");
            QUIT { default { @events.push("quit(" ~ .Str ~ ")") } }
            LAST { @events.push("last") }
        }
    }

    is-deeply @events, ["value(1)", "quit(boom)"], 'react/whenever handles supply quit without running LAST';
}
