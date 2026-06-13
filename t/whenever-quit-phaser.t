use Test;
plan 6;

# A QUIT phaser inside a `whenever` runs when the source quits. If it handles
# the exception (a when/default matches), the supply completes with `done` and
# the downstream quit handler is suppressed; an emit inside it forwards to the
# supply. See roast/S17-supply/syntax.t.

# QUIT default handles any exception -> supply is done, no quit.
{
    my $trigger = Supplier.new;
    my $d = 0; my $q = 0;
    my $s = supply {
        whenever $trigger { QUIT { default { } } }
    }
    $s.tap(done => { $d++ }, quit => { $q++ });
    $trigger.quit(Exception.new);
    is $d, 1, 'handled QUIT (default) completes the supply with done';
    is $q, 0, 'handled QUIT does not fire the downstream quit';
}

# emit inside a QUIT block forwards to the supply's tap.
{
    my class OMGBears is Exception { }
    my $trigger = Supplier.new;
    my @got;
    my $s = supply {
        whenever $trigger {
            emit $_;
            QUIT { when OMGBears { emit "Run!"; } }
        }
    }
    $s.tap({ @got.push($_) });
    $trigger.emit("A");
    $trigger.emit("B");
    $trigger.quit(OMGBears.new);
    is @got, ["A", "B", "Run!"], 'emit inside a matched QUIT block forwards to the supply';
}

# An unmatched QUIT still quits (the exception is not handled).
{
    my class Useless is Exception { }
    my $trigger = Supplier.new;
    my $d = 0; my $q = 0;
    my $s = supply {
        whenever $trigger { QUIT { when Useless { } } }
    }
    $s.tap(done => { $d++ }, quit => { $q++ });
    $trigger.quit(Exception.new);
    is $q, 1, 'unmatched QUIT still quits';
    is $d, 0, '...and does not run done';
}

# A whenever with no QUIT phaser propagates the quit to the tap.
{
    my $trigger = Supplier.new;
    my $q = 0;
    my $s = supply { whenever $trigger { } }
    $s.tap(quit => { $q++ });
    $trigger.quit('boom');
    is $q, 1, 'quit propagates when there is no QUIT phaser';
}
