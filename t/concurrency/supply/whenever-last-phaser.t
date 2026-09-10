use Test;
plan 4;

# A LAST phaser inside a `whenever` block must run when the source supply
# is done, and any `emit` inside it must forward to the enclosing supply.
# See roast/S17-supply/syntax.t ("Value from LAST block correct").

{
    my $trigger = Supplier.new;
    my $s = supply {
        whenever $trigger -> $value {
            emit $value;
            LAST { emit "END"; }
        }
    }
    my @got;
    $s.tap({ @got.push($_) });
    $trigger.emit("a");
    $trigger.emit("b");
    $trigger.done();
    is @got, ["a", "b", "END"], 'LAST phaser emit forwards to supply on done';
}

# LAST runs even with a done callback present on the tap.
{
    my $trigger = Supplier.new;
    my $done = 0;
    my $s = supply {
        whenever $trigger -> $value {
            emit $value;
            LAST { emit "fin"; }
        }
    }
    my @got;
    $s.tap({ @got.push($_) }, done => { $done++ });
    $trigger.emit(1);
    $trigger.done();
    is @got, [1, "fin"], 'LAST emit forwards with a done tap callback';
    is $done, 1, 'done callback still fires once after LAST';
}

# A LAST phaser that only runs side effects (no emit) still executes.
{
    my $trigger = Supplier.new;
    my $ran = 0;
    my $s = supply {
        whenever $trigger -> $value {
            emit $value;
            LAST { $ran++; }
        }
    }
    my @got;
    $s.tap({ @got.push($_) });
    $trigger.emit("x");
    $trigger.done();
    is $ran, 1, 'LAST side-effect block runs on done';
}
