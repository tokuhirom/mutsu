use Test;

# Writes that do not fit a fixed-dimension array die with rakudo's exceptions
# and messages instead of dropping items or saying "Index out of bounds".
# Expectations measured against rakudo 2026.09.

plan 15;

sub err(&code) { try code(); my $e = $!; $! = Nil; $e }

{
    my @d[2];
    try { @d = 1, 2, 3 }
    is $!.^name, 'X::AdHoc', 'too many items for a one-dimension array';
    is $!.message, 'Index 2 for dimension 1 out of range (must be 0..1)', 'names the first index past the end';
    is @d.elems, 2, 'and the array keeps its shape';
}

{
    my @d[3];
    @d = 1, 2;
    is @d.raku, 'Array.new(:shape(3,), [1, 2, Any])', 'fewer items still fill from the start';
}

{
    my @d[2];
    is err({ @d[2] = 1 }).message, 'Index 2 for dimension 1 out of range (must be 0..1)',
        'a write past the end of a one-dimension array';
    is err({ @d[5] = 1 }).message, 'Index 5 for dimension 1 out of range (must be 0..1)',
        'a write far past the end';
}

{
    my @d[2;2];
    is err({ @d[2;0] = 1 }).message, 'Index 2 for dimension 1 out of range (must be 0..1)',
        'a write past the first dimension';
    is err({ @d[0;2] = 1 }).message, 'Index 2 for dimension 2 out of range (must be 0..1)',
        'a write past the second dimension';
    my $e = err({ @d[0] = 1 });
    is $e.^name, 'X::NotEnoughDimensions', 'too few indices is X::NotEnoughDimensions';
    is $e.message, 'Cannot assign to a 2 dimension array with only 1 dimensions', 'with rakudo\'s message';
    is $e.operation, 'assign to', 'and its operation attribute';
    is $e.needed-dimensions, 2, 'the needed dimensions';
    is $e.got-dimensions, 1, 'the dimensions got';
}

{
    my @d[2;3];
    try { @d = (1, 2, 3), 4 }
    is $!.^name, 'X::Assignment::ToShaped', 'a flat item among rows is ToShaped';
    is $!.message, 'Assignment to array with shape 2 3 must provide structured data', 'with rakudo\'s message';
}
