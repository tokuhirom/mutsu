use Test;

plan 10;

# ADR-0031: a `whenever` body's own `die` belongs to the enclosing `supply`
# block's own emitter, not to whichever upstream source happened to dispatch
# the callback. `call_supply_tap` converts a stamped whenever body's
# non-control `Err` into `$emitter.quit($reason)` so every whenever-source
# shape (supplier-backed, channel-backed, chained on-demand, and a nested
# `whenever <Promise>`) tears the block down the same way, and a source's own
# quit still runs that whenever's QUIT phaser first (unaffected).

# The deep ticket's own repro (todo/deep/cold-supply-whenever-source-replayed-not-tapped.md).
{
    sub timeout($source, $timeout) {
        supply {
            whenever $source -> $value {
                state $values++;
                emit $value;
                my $last-values = $values;
                whenever Promise.in($timeout) {
                    if $last-values == $values { die "Timed out" }
                }
            }
        }
    }

    my $test-source = supply {
        for 0.05, 0.10, 0.25 { whenever Promise.in($_) { emit 'badger' } }
    }
    my $timed-out = timeout($test-source, 0.10);
    my @received;
    my $died = False;
    $timed-out.tap: { @received.push($_) }, quit => { $died = True }
    sleep 0.5;
    is @received, ['badger', 'badger'],
        "the deep ticket repro delivers only the values before the timeout";
    ok $died, "the deep ticket repro quits the tap";
}

# probe3 case C: die in a whenever body whose source is a cold on-demand supply
# (the b3 "chained on-demand source" branch, which previously registered the
# tap's quit => nowhere at all).
{
    my $cold = supply { emit 1; emit 2; emit 3; }
    my $died = False;
    my @received;
    my $src = supply {
        whenever $cold -> $v {
            @received.push($v);
            die "boom" if $v == 2;
        }
    }
    $src.tap: {;}, quit => { $died = True }
    is @received, [1, 2], "a body die stops a chained on-demand source after the dying value";
    ok $died, "a body die on a chained on-demand source quits the tap";
}

# probe3 case B: die in a nested `whenever <Promise>` body — registered from
# inside another whenever's body, after the enclosing supply block's own
# synchronous run is over.
{
    my $died = False;
    my $src = supply {
        whenever Promise.in(0.5) { emit 'x' }
        whenever Promise.in(0.05) {
            whenever Promise.in(0.05) {
                die "nested boom";
            }
        }
    }
    $src.tap: {;}, quit => { $died = True }
    sleep 0.3;
    ok $died, "a die in a nested whenever <Promise> body quits the enclosing block";
}

# probe6 case F (negative pin): a body die does NOT run that same whenever's
# own QUIT phaser — it goes straight to the tap's `quit =>` handler.
{
    my $sup = Supplier.new;
    my $quit-phaser-ran = False;
    my $died = False;
    my $src = supply {
        whenever $sup.Supply -> $v {
            die "boom";
            QUIT { $quit-phaser-ran = True; }
        }
    }
    $src.tap: {;}, quit => { $died = True }
    $sup.emit(1);
    sleep 0.1;
    nok $quit-phaser-ran, "a body die does not run the whenever's own QUIT phaser";
    ok $died, "a body die still reaches the tap's quit =>";
}

# probe6 case G (negative pin): a *source* quit does run the whenever's own
# QUIT phaser first; when unhandled it still reaches the tap's quit =>, and
# when handled it suppresses the downstream quit — proving the new
# body-die-to-quit conversion did not also reroute genuine source quits.
{
    my $sup = Supplier.new;
    my $quit-phaser-ran = False;
    my $died = False;
    my $src = supply {
        whenever $sup.Supply -> $v {
            QUIT { $quit-phaser-ran = True; }
        }
    }
    $src.tap: {;}, quit => { $died = True }
    $sup.quit("source boom");
    sleep 0.1;
    ok $quit-phaser-ran, "a source quit runs the whenever's own QUIT phaser";
    ok $died, "an unhandled source-quit QUIT phaser still reaches the tap's quit =>";
}
{
    my $sup = Supplier.new;
    my $died = False;
    my $src = supply {
        whenever $sup.Supply -> $v {
            QUIT { default { } }
        }
    }
    $src.tap: {;}, quit => { $died = True }
    $sup.quit("source boom");
    sleep 0.1;
    nok $died, "a handled source-quit QUIT phaser suppresses the tap's quit =>";
}
