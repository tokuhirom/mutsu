use Test;

plan 6;

# Basic list assignment to pre-existing variables
{
    my $a = 0;
    my $b = 0;
    ($a, $b) = (3, 4);
    is $a, 3, 'list assign to existing vars: $a';
    is $b, 4, 'list assign to existing vars: $b';
}

# List assignment from a function returning a list
{
    sub pair-vals { return (10, 20) }
    my $x;
    my $y;
    ($x, $y) = pair-vals();
    is $x, 10, 'list assign from function: $x';
    is $y, 20, 'list assign from function: $y';
}

# List assignment with more values than targets
{
    my $a;
    my $b;
    ($a, $b) = (1, 2, 3);
    is $a, 1, 'extra values: $a gets first';
    is $b, 2, 'extra values: $b gets second';
}
