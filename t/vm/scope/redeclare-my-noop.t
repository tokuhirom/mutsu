use Test;

# Redeclaring an existing same-scope `my` variable WITHOUT an explicit
# initializer is a no-op in Raku: the variable keeps its current value
# (only a "Redeclaration of symbol" warning is emitted). A redeclaration
# WITH an initializer still runs the assignment.

plan 9;

# bare redeclaration preserves the value
{
    my $f;
    $f = 5;
    my $f; #OK
    is $f, 5, 'bare redeclaration of $f preserves value';
}

# typed redeclaration without initializer is also a no-op
{
    my $g = 7;
    my Int $g; #OK
    is $g, 7, 'typed redeclaration without initializer preserves value';
}

# redeclaration WITH an initializer runs the assignment
{
    my $h = 1;
    my $h = 2; #OK
    is $h, 2, 'redeclaration with initializer runs assignment';
}

# a fresh declaration still resets to the type default
{
    my $x = 9;
    is $x, 9, 'fresh declaration with initializer';
    my $y;
    nok $y.defined, 'fresh bare declaration is undefined';
}

# nested-block shadowing is unaffected (inner is a fresh, undefined var)
{
    my $z = 3;
    {
        my $z; #OK
        nok $z.defined, 'inner shadow is a fresh undefined var';
    }
    is $z, 3, 'outer value restored after shadow block';
}

# array/hash bare redeclaration preserves contents
{
    my @a = 1, 2, 3;
    my @a; #OK
    is @a.join(','), '1,2,3', 'bare @-redeclaration preserves elements';

    my %m = a => 1;
    my %m; #OK
    is %m<a>, 1, 'bare %-redeclaration preserves pairs';
}
