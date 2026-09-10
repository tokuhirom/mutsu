use Test;

plan 8;

# Binding to array element - changes through original propagate to bound variable
{
    my @a = 1, 2, 3;
    my $x := @a[1];
    is $x, 2, 'bound array element has initial value';
    @a[1] = 99;
    is $x, 99, 'bound array element sees modification through original';
}

# Binding to hash element - changes through original propagate to bound variable
{
    my %h = a => 1, b => 2;
    my $x := %h<a>;
    is $x, 1, 'bound hash element has initial value';
    %h<a> = 99;
    is $x, 99, 'bound hash element sees modification through original';
}

# COW semantics: array assignment creates independent copy
{
    my @a = 1, 2, 3;
    my @b = @a;
    @b[0] = 99;
    is @a[0], 1, 'array assignment creates independent copy';
    is @b[0], 99, 'modified copy has new value';
}

# is copy parameter creates independent copy
{
    sub f(@arr is copy) {
        @arr[0] = 99;
    }
    my @a = 1, 2, 3;
    f(@a);
    is @a[0], 1, 'is copy array parameter does not modify caller';
}

# is copy hash parameter creates independent copy
{
    sub g(%h is copy) {
        %h<a> = 99;
    }
    my %h = a => 1;
    g(%h);
    is %h<a>, 1, 'is copy hash parameter does not modify caller';
}
