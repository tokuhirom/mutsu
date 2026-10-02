# `@a.BIND-POS($i, 42)` binds the element to a bare value, which has no
# container: a later assignment to it dies with "Cannot assign to an immutable
# value", whichever way the assignment is spelled (#10924). The element is a
# read-only cell, as `%h.BIND-KEY($k, 42)` makes one for a hash.
use Test;

plan 12;

{
    my @b = 1, 2, 3;
    @b.BIND-POS(1, 42);
    throws-like { @b[1] = 3 }, X::AdHoc, message => 'Cannot assign to an immutable value',
        '[]= on a BIND-POS-bound element dies';
    is-deeply @b, [1, 42, 3], 'the bound value survives';
    my $i = 1;
    throws-like { @b[$i] = 3 }, X::AdHoc, message => 'Cannot assign to an immutable value',
        'a run-time index dies too';
    throws-like { @b.ASSIGN-POS(1, 3) }, X::AdHoc,
        message => 'Cannot assign to an immutable value', 'ASSIGN-POS dies the same way';
    throws-like { $_ = 0 for @b }, X::AdHoc, message => 'Cannot assign to an immutable value',
        'so does a for loop alias';
    @b[0] = 9;
    is-deeply @b, [9, 42, 3], 'other elements stay assignable';
    is @b.map(* + 1).join(','), '10,43,4', 'the bound value reads normally';
}

{
    my @c = 1, 2, 3;
    @c.BIND-POS(1, 42);
    @c[1] := 7;
    throws-like { @c[1] = 8 }, X::AdHoc, 'a rebind to another literal stays immutable';
    my @b = 1, 2, 3;
    @b.BIND-POS(1, 42);
    my $x = 5;
    @b[1] := $x;
    @b[1] = 6;
    is $x, 6, 'a rebind to a variable makes the element writable again';
}

{
    my @b = 1, 2, 3;
    my $x = 5;
    @b.BIND-POS(1, $x);
    $x = 9;
    is @b[1], 9, 'BIND-POS to a variable still aliases it';
    @b[1] = 7;
    is $x, 7, 'and writes through the element reach the variable';
}

{
    my @b = 1, 2;
    @b.BIND-POS(4, 42);
    is @b.raku, '[1, 2, Any, Any, 42]', 'binding past the end grows the array';
}
