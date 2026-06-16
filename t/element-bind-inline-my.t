use Test;

# Binding an array/hash element to an inline-declared scalar
# (`@a[1] := my $x`) must share a container cell between the element and the
# freshly-declared variable, so writes through either side are visible on the
# other -- matching Rakudo. Previously the inline `my` declaration's name was
# lost, so the element snapshotted Nil and never shared a cell.

plan 11;

# Array element <- inline my, write through the variable
{
    my @a = 1, 2, 3;
    @a[1] := my $x;
    $x = 99;
    is @a[1], 99, 'write through $x reaches @a[1]';
    is @a.join(','), '1,99,3', 'array reflects the bound write';
}

# Array element <- inline my, write through the element (bidirectional)
{
    my @a = 1, 2, 3;
    @a[1] := my $x;
    @a[1] = 5;
    is $x, 5, 'write through @a[1] reaches $x';
}

# Hash element <- inline my
{
    my %h;
    %h<k> := my $v;
    $v = 42;
    is %h<k>, 42, 'write through $v reaches %h<k>';
    %h<k> = 7;
    is $v, 7, 'write through %h<k> reaches $v';
}

# The inline-declared variable is visible in the enclosing scope
{
    my @a = 1, 2, 3;
    @a[1] := my $x;
    ok $x.defined.not, 'inline $x starts undefined (bound to a fresh cell)';
    $x = 11;
    is $x, 11, '$x is a normal lexical after the bind';
}

# Pre-declared element bind still works (regression guard)
{
    my @a = 1, 2, 3;
    my $x;
    @a[1] := $x;
    $x = 99;
    is @a[1], 99, 'pre-declared element bind still shares a cell';
}

# Autovivified array element via inline my
{
    my @a;
    @a[2] := my $y;
    $y = 'z';
    is @a[2], 'z', 'autovivified element binds to inline my';
    is @a.elems, 3, 'array grew to hold the bound element';
}

# Multi-element independent binds
{
    my @a = 0, 0, 0;
    @a[0] := my $p;
    @a[2] := my $q;
    $p = 'a';
    $q = 'c';
    is @a.join(','), 'a,0,c', 'independent element binds do not interfere';
}
