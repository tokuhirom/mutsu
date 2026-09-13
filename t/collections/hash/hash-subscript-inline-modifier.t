use Test;

plan 4;

{
    my %m;
    my $n = 'ab';
    %m{S/b/c/ with $n} = 1;
    is-deeply %m, { ac => 1 },
        'a statement modifier in a hash subscript evaluates as part of the key';
}

{
    my %m;
    %m{1 if True} = 'yes';
    is %m{1}, 'yes',
        'a conditional modifier in a hash subscript is accepted';
}

{
    my @values;
    @values[1 if True] = 'yes';
    is @values[1], 'yes',
        'the shared bracket parser also accepts a modifier in an array subscript';
}

{
    my @values;
    @values[1 if False] = 'yes';
    ok @values[1].not.defined,
        'a false subscript modifier produces an undefined index value';
}
