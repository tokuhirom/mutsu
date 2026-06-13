use Test;
plan 6;

# A CLOSE phaser in a `supply { ... }` block runs once when the tap is closed
# or the supply terminates normally — whichever happens first.
# See roast/S17-supply/syntax.t.

# CLOSE runs on an explicit tap close (and not before).
{
    my $closed = False;
    my $input = Supplier.new;
    my $s = supply {
        whenever $input { emit .uc; }
        CLOSE { $closed = True; }
    }
    my @got;
    my $t = $s.tap({ @got.push: $_ });
    $input.emit('dugong!');
    is @got, ['DUGONG!'], 'supply with a CLOSE phaser emits as normal';
    nok $closed, 'CLOSE phaser has not run yet';
    $t.close;
    ok $closed, 'CLOSE phaser runs on tap close';
}

# CLOSE runs on normal termination (source done), and not twice afterwards.
{
    my $closed = False;
    my $input = Supplier.new;
    my $s = supply {
        whenever $input { }
        CLOSE { $closed = True; }
    }
    my $t = $s.tap();
    $input.done();
    ok $closed, 'CLOSE phaser runs on normal termination (done)';
    $closed = False;
    $t.close;
    nok $closed, 'CLOSE phaser does not run again on a later tap close';
}

# CLOSE sees its enclosing lexical scope.
{
    my $total = 0;
    sub make-supply($n) {
        supply {
            whenever $n.Supply { }
            CLOSE { $total += $n; }
        }
    }
    my $sup = Supplier.new;
    make-supply(7).tap.close;
    is $total, 7, 'CLOSE phaser sees the enclosing scope';
}
