use Test;

plan 5;

# Basic: whenever block receives emitted value, not the Supplier object
{
    my $sup = Supplier.new;
    my @collected;
    my $s = supply { whenever $sup { emit $_ * 2; } };
    $s.tap({ @collected.push($_) });
    $sup.emit(21);
    is @collected[0], 42, 'whenever block receives emitted value via $_';
}

# Multiple emits
{
    my $sup = Supplier.new;
    my @collected;
    my $s = supply { whenever $sup { emit $_ + 1; } };
    $s.tap({ @collected.push($_) });
    $sup.emit(10);
    $sup.emit(20);
    $sup.emit(30);
    is @collected.elems, 3, 'multiple emits all received';
    is @collected.join(','), '11,21,31', 'all emitted values transformed correctly';
}

# Explicit parameter
{
    my $sup = Supplier.new;
    my @collected;
    my $s = supply { whenever $sup -> $val { emit $val * 3; } };
    $s.tap({ @collected.push($_) });
    $sup.emit(7);
    is @collected[0], 21, 'whenever with explicit param receives emitted value';
}

# Supplier::Preserving
{
    my $sup = Supplier::Preserving.new;
    my @collected;
    my $s = supply { whenever $sup { emit $_ * 2; } };
    $s.tap({ @collected.push($_) });
    $sup.emit(5);
    is @collected[0], 10, 'Supplier::Preserving emitted value received in whenever';
}
