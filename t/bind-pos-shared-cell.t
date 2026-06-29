use Test;

plan 9;

# `@a.BIND-POS($i, $x)` binds element $i to the caller variable $x as a shared
# container cell: a later `$x = ...` writes through to `@a[$i]` and vice versa.
# This is the array analog of `%h.BIND-KEY` (which already shares a cell).

{
    my @a = 42, 666;
    my $x = 45;
    is @a.BIND-POS(0, $x), 45, 'BIND-POS returns the bound value';
    is @a.AT-POS(0), 45, 'element reads the bound value';
    $x = 90;
    is @a.AT-POS(0), 90, 'mutating the source variable writes through the element';
}

# BIND-POS to an index past the end extends the array.
{
    my @a = 1, 2;
    my $d = 67;
    @a.BIND-POS(3, $d);
    is @a.AT-POS(3), 67, 'BIND-POS extends the array';
    $d = 56;
    is @a.AT-POS(3), 56, 'extended bound element tracks the source';
}

# Writing through the element updates the source variable too (shared cell).
{
    my @a = 1, 2, 3;
    my $x = 10;
    @a.BIND-POS(1, $x);
    @a[1] = 99;
    is $x, 99, 'assigning the element writes back to the bound variable';
}

# `@a[$i] := $x` (the binding operator) and BIND-POS share the same mechanism.
{
    my @a = 1, 2;
    my $x = 7;
    @a[0] := $x;
    $x = 8;
    is @a[0], 8, ':= element binding aliases the variable';
}

# A subscript expression still reads the bound value transparently.
{
    my @a = 5, 6;
    my $x = 100;
    @a.BIND-POS(0, $x);
    is @a[0] + 1, 101, 'bound element decontainerizes in value context';
    is @a.elems, 2, 'BIND-POS in place does not change elems';
}
