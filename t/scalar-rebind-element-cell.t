use Test;

# Binding an already-declared scalar to an array/hash element in a *separate*
# statement (`my $x; $x := @a[1]`) must share a cell with the element, so a
# later `$x = v` writes through to `@a[1]` — exactly like the single-statement
# `my $x := @a[1]` form. Previously the standalone rebind only snapshotted the
# element value, so the write never reached the container.

plan 9;

{
    my @a = 1, 2, 3;
    my $x;
    $x := @a[1];
    $x = 5;
    is-deeply @a, [1, 5, 3], 'rebind $x := @a[1] writes through to the array';
    is $x, 5, 'and $x reads the shared value';
}

{
    my %h = a => 1, b => 2;
    my $x;
    $x := %h<a>;
    $x = 9;
    is %h<a>, 9, 'rebind $x := %h<a> writes through to the hash';
}

{
    my @a = [1, 2], [3, 4];
    my $x;
    $x := @a[0][1];
    $x = 99;
    is-deeply @a, [[1, 99], [3, 4]], 'rebind to a nested element writes through';
}

# Single-statement form is unchanged.
{
    my @a = 1, 2, 3;
    my $r := @a[1];
    $r = 99;
    is-deeply @a, [1, 99, 3], 'my $r := @a[1] still works';
}

# Re-rebinding to a different element follows the new target.
{
    my @a = 1, 2, 3;
    my $x := @a[0];
    $x := @a[2];
    $x = 99;
    is-deeply @a, [1, 2, 99], 're-rebind retargets the cell';
}

# Var-to-var rebind is unaffected (no element/cell promotion needed, but must
# still alias).
{
    my $y = 10;
    my $x;
    $x := $y;
    $x = 5;
    is $y, 5, 'scalar-to-scalar rebind still aliases';
}

# Element bound through a rebind reads the current element value.
{
    my @a = 7, 8, 9;
    my $x;
    $x := @a[2];
    is $x, 9, 'rebound element reads its current value';
    @a[2] = 100;
    is $x, 100, 'and reflects a later write to the element';
}
