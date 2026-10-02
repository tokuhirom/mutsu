use Test;

# A CLOSE phaser in a `supply { }` block closes over the block's own lexicals,
# including ones declared (and initialised) before it (issue #10832). Its
# registration is still hoisted ahead of a loop that follows those
# declarations, so it can stop that loop.

plan 6;

{
    my $s = Supplier.new;
    my $n = 0;
    my @closed;
    my $sup = supply {
        my $id = ++$n;
        CLOSE { @closed.push($id) }
        whenever $s.Supply { emit $_ }
    };
    $n = 41;
    $sup.tap.close;
    is-deeply @closed, [42], 'CLOSE sees a scalar declared before it';
}

{
    my $s = Supplier.new;
    my $seen;
    my $sup = supply {
        my @parts = <a b>;
        my %h = k => 'v';
        CLOSE { $seen = "@parts[] %h<k>" }
        whenever $s.Supply { emit $_ }
    };
    $sup.tap.close;
    is $seen, 'a b v', 'CLOSE sees an array and a hash declared before it';
}

{
    my $s = Supplier.new;
    my $seen;
    my $sup = supply {
        my ($a, $b) = 1, 2;
        CLOSE { $seen = $a + $b }
        whenever $s.Supply { emit $_ }
    };
    $sup.tap.close;
    is $seen, 3, 'CLOSE sees variables from a list declaration';
}

{
    my $s = Supplier.new;
    my $seen;
    my $sup = supply {
        my $id = 7;
        whenever $s.Supply { emit $_ }
        CLOSE { $seen = $id }
    };
    $sup.tap.close;
    is $seen, 7, 'a CLOSE written after the whenever sees the declaration too';
}

{
    # A CLOSE after a loop still runs while the loop is going: it must be
    # registered ahead of the loop to be able to stop it.
    my $tap;
    my @got;
    my $sup = supply {
        my $stop = False;
        my $i = 0;
        until $stop {
            emit ++$i;
            $tap.close if $i == 3 && $tap;
            last if $i > 10;
        }
        CLOSE { $stop = True }
    };
    $tap = $sup.tap({ @got.push($_) });
    ok @got.elems >= 3, 'the loop emitted';
    ok @got.elems <= 11, 'the loop finished';
}
