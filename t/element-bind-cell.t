use Test;

# Phase 2 (element containers): binding a scalar to an array/hash element
# (`$x := @a[i]`) aliases the element by a shared cell, so writes through
# either side are visible to the other — including through arbitrarily deep
# `$struct[..]<..>[..]` paths (which the old index-back-reference lost when an
# enclosing container was COW-cloned on a later write).

plan 13;

# Single-level array element
{
    my @a = 1, 2, 3;
    my $x := @a[0];
    is $x, 1, 'bound element reads the current value';
    @a[0] = 99;
    is $x, 99, 'write to the element is visible through the binding';
    $x = 5;
    is @a[0], 5, 'write through the binding is visible in the element';
}

# Two-level array
{
    my $s = [0, [10, 20]];
    my $x := $s[1][1];
    is $x, 20, 'deep (2-level) bound element reads';
    $s[1][1] = 99;
    is $x, 99, '2-level: element write -> binding';
    $x = 5;
    is $s[1][1], 5, '2-level: binding write -> element';
}

# Deep mixed array/hash path (the nested.t shape)
{
    my $struct = [
        "ignored",
        { key => { subkey => [ "ignored", 42 ] } },
    ];
    my $abbrev := $struct[1]<key><subkey>[1];
    is $abbrev, 42, 'deep array/hash path bound element reads';
    $struct[1]<key><subkey>[1] = 43;
    is $abbrev, 43, 'deep: element write -> binding (survives COW of the path)';
    $abbrev = 44;
    is $struct[1]<key><subkey>[1], 44, 'deep: binding write -> element';
}

# The bound element stays transparent in value contexts (no cell leak)
{
    my @a = 1, 2, 3;
    my $x := @a[1];
    is @a.sum, 6, 'sum sees the element value, not the cell';
    is @a.reduce(&[+]), 6, 'reduce sees the element value';
    is @a.raku, '[1, 2, 3]', '.raku does not leak the cell';
    is (@a[1] + 10), 12, 'arith on the element reads through';
}

# vim: expandtab shiftwidth=4
