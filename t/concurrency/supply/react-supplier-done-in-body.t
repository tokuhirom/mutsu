use Test;

plan 4;

# #12209: a Supplier ended from inside the react body must end the react,
# with the values emitted before it delivered first.
{
    my @got; my $s = Supplier.new;
    react { my $t = do whenever $s.Supply -> $x { @got.push($x) }; $s.emit(1); $s.emit(2); $s.done; }
    is @got.join(','), '1,2', 'do whenever + $s.done in the react body';
}
{
    my @got; my $s = Supplier.new;
    react { whenever $s.Supply -> $x { @got.push($x) }; $s.emit(1); $s.done; }
    is @got.join(','), '1', 'plain whenever + $s.done in the react body';
}
{
    my $s = Supplier.new;
    my $last = 0;
    react { whenever $s.Supply { LAST $last++ }; $s.done; }
    is $last, 1, 'LAST phaser runs when done precedes any emit';
}
{
    my $s = Supplier.new;
    my @got;
    react { whenever $s.Supply -> $x { @got.push($x) }; $s.done; }
    my $tapped = 0;
    $s.Supply.tap({ $tapped++ });
    $s.emit(5);
    is $tapped, 1, 'the supplier is reusable after the react ended';
}
