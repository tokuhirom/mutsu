use Test;

plan 3;

# `Promise(supply { whenever <derived-supply> -> $x { ...; LAST emit ... } })`
# lost every value once the `whenever`'s source was itself a DERIVED supply —
# a `supply { whenever $raw -> $x { emit $x } }` block — rather than a raw
# `Supplier.Supply`. The Promise-coercion classifier
# (`supply_promise_on_demand`) had no branch for a nested on-demand source, so
# it fell through to a synchronous static replay that silently drops a still-
# live subscription. See
# todo/deep/last-phaser-loses-outer-var-mutations-when-whenever-source-is-a-nested-supply.md.

{
    # The ticket's original repro: accumulate across two async emits, read the
    # accumulator from the LAST phaser.
    my $source = Supplier.new;
    my $inner = supply {
        whenever $source.Supply -> $x {
            emit $x;
        }
    }
    my $p = Promise(supply {
        my $joined = 0;
        whenever $inner -> $x {
            $joined += $x;
            LAST emit $joined;
        }
    });
    start {
        $source.emit(1);
        $source.emit(2);
        $source.done;
    }
    await Promise.anyof($p, Promise.in(5));
    is $p.result, 3, 'LAST phaser sees mutations made within the same nested-supply whenever body';
}

{
    # No LAST at all: an ordinary `emit` from the nested-supply whenever body
    # must still reach the Promise.
    my $source = Supplier.new;
    my $inner = supply {
        whenever $source.Supply -> $x {
            emit $x;
        }
    }
    my $p = Promise(supply {
        whenever $inner -> $x {
            emit $x;
        }
    });
    start {
        $source.emit(1);
        $source.done;
    }
    await Promise.anyof($p, Promise.in(5));
    is $p.result, 1, 'a plain emit from a nested-supply whenever body reaches the Promise';
}

{
    # LAST phaser with no per-value body at all (the whole whenever body is
    # the LAST phaser) must still fire once the nested source completes.
    my $source = Supplier.new;
    my $inner = supply {
        whenever $source.Supply -> $x {
            emit $x;
        }
    }
    my $p = Promise(supply {
        whenever $inner -> $x {
            LAST emit 99;
        }
    });
    start {
        $source.emit(1);
        $source.emit(2);
        $source.done;
    }
    await Promise.anyof($p, Promise.in(5));
    is $p.result, 99, 'a LAST-only nested-supply whenever body fires its LAST phaser';
}
