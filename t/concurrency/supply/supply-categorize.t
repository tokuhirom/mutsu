use Test;

plan 8;

# .categorize / .classify cannot be called on the Supply type object.
dies-ok { Supply.categorize({ ... }) }, 'Supply.categorize dies as a class method';
dies-ok { Supply.classify({ ... }) },  'Supply.classify dies as a class method';

# Live .categorize with a Block mapper returning a list of keys.
# A value goes to every key in the list (or to none for an empty list).
{
    my &mapper = { $_ div 10 ?? (0, 1) !! () };
    my $s = Supplier.new;
    my $c = $s.Supply.categorize(&mapper);
    ok $c ~~ Supply, 'categorize returns a Supply';

    my @keys;
    my @supplies;
    $c.tap(-> $p { @keys.push: $p.key; @supplies.push: $p.value });

    my @bucket0;
    my @bucket1;
    $s.emit($_) for 1, 2, 3, 11, 12, 13;
    $s.done;

    is-deeply @keys, [0, 1], 'categorize emits a Pair per bucket key';
    is @supplies.elems, 2, 'two classification sub-supplies';
}

# Live .classify with a Block mapper: one key per value.
{
    my &mapper = { $_ div 10 };
    my $s = Supplier.new;
    my @keys;
    $s.Supply.classify(&mapper).tap(-> $p { @keys.push: $p.key });
    $s.emit($_) for 1, 2, 3, 11, 12, 13;
    $s.done;
    is-deeply @keys, [0, 1], 'classify emits one key bucket per distinct key';
}

# Hash mapper honors `is default(...)` for missing keys.
{
    my %mapper is default(()) = (11 => (0, 1), 12 => (0, 1), 13 => (0, 1));
    my $s = Supplier.new;
    my @keys;
    $s.Supply.categorize(%mapper).tap(-> $p { @keys.push: $p.key });
    $s.emit($_) for 1, 2, 3, 11, 12, 13;
    $s.done;
    is-deeply @keys, [0, 1], 'categorize hash mapper: default(()) drops uncategorized values';
}

# Array mapper: index lookup returning a list of keys.
{
    my @mapper = (), (), (), (), (), (), (), (), (), (),
        $(0, 1), $(0, 1), $(0, 1), $(0, 1), $(0, 1),
        $(0, 1), $(0, 1), $(0, 1), $(0, 1), $(0, 1);
    my $s = Supplier.new;
    my @keys;
    $s.Supply.categorize(@mapper).tap(-> $p { @keys.push: $p.key });
    $s.emit($_) for 1, 2, 3, 11, 12, 13;
    $s.done;
    is-deeply @keys, [0, 1], 'categorize array mapper: itemized list keys';
}
