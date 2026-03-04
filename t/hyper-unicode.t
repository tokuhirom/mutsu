use Test;

plan 10;

# Basic Unicode hyper infix operators with guillemets
{
    my @a = 1, 2, 3;
    my @b = 10, 20, 30;

    is (@a >>+<< @b).join(','), '11,22,33', '>>+<< ASCII works';
    is (@a >>+>> @b).join(','), '11,22,33', '>>+>> ASCII works';

    # All four Unicode guillemet forms
    is (@a »+« @b).join(','), '11,22,33', 'guillemet right-op-left works';
    is (@a »+» @b).join(','), '11,22,33', 'guillemet right-op-right works';
    is (@a «+« @b).join(','), '11,22,33', 'guillemet left-op-left works';
    is (@a «+» @b).join(','), '11,22,33', 'guillemet left-op-right works';
}

# Unicode hyper with subtraction
{
    my @a = 10, 20, 30;
    my @b = 1, 2, 3;
    is (@a »-« @b).join(','), '9,18,27', 'guillemet subtraction works';
}

# Unicode hyper with multiplication
{
    my @a = 2, 3, 4;
    my @b = 10, 10, 10;
    is (@a »*« @b).join(','), '20,30,40', 'guillemet multiplication works';
}

# DWIM behavior with Unicode hyper
{
    my @a = 1, 2, 3, 4;
    my @b = 10;
    is (@a »+» @b).join(','), '11,12,13,14', 'guillemet DWIM right extends shorter list';
}

# Unicode hyper with string op
{
    my @a = "a", "b", "c";
    my @b = "x", "y", "z";
    is (@a »~« @b).join(','), 'ax,by,cz', 'guillemet string concat';
}
