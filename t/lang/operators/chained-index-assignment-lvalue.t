# A parenthesized assignment is itself an lvalue: `($x = 5) = 6` writes through
# to `$x` again. This must work for an index/slice assignment target too
# (`(@a[i] = v) = w`), not just a scalar variable — the "hat trick" from
# roast/S02-types/array.t. The inner assignment runs for its side effect, then
# the outer RHS is assigned to the SAME subscript.
use Test;

plan 9;

# Scalar chained assignment (baseline — already worked).
{
    my $x;
    ($x = 5) = 6;
    is $x, 6, 'scalar ($x = 5) = 6 writes through';
}

# Single array element as a chained-assignment lvalue.
{
    my @a = 0, 0, 0;
    (@a[0] = 5) = 7;
    is @a[0], 7, '(@a[0] = 5) = 7 writes through to @a[0]';
    is @a[1], 0, 'other elements untouched';
}

# Single hash element as a chained-assignment lvalue.
{
    my %h;
    (%h<k> = 1) = 2;
    is %h<k>, 2, '(%h<k> = 1) = 2 writes through to %h<k>';
}

# Slice as a chained-assignment lvalue.
{
    my @b = 0, 0, 0, 0;
    (@b[0, 1] = 1, 2) = 8, 9;
    is ~@b, '8 9 0 0', '(@b[0,1] = 1,2) = 8,9 writes the slice';
}

# The S02-types/array.t "hat trick": assign to an end-indexed slice, then use
# that as an lvalue for a reversed end-indexed slice of the source.
{
    my @array14 = 'a', 'b', 'c', 'd';
    my @c = 0 .. 3;
    ((@c[*-3, *-2, *-1, *-4] = @array14) = @array14[*-1, *-2, *-3, *-4]);
    is ~@c, 'a d c b', 'hat trick: chained end-indexed slice lvalue';
}

# The chained assignment is an expression: its value is the final assignment.
{
    my @d = 0, 0;
    my $r = ((@d[0] = 1) = 42);
    is $r, 42, 'chained index assignment yields the final assigned value';
    is @d[0], 42, '...and @d[0] holds it';
}

# Nested-container element chained assignment.
{
    my @aoa = [0, 0], [0, 0];
    (@aoa[0][1] = 5) = 9;
    is @aoa[0][1], 9, 'chained assignment through a nested element writes through';
}
