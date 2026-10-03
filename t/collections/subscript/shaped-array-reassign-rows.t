use Test;

plan 9;

# Array::Shaped::Console's test file: a shaped declaration in a unit that
# also has BEGIN-time statements keeps its shape (the BEGIN prologue used to
# split it into an empty array plus a plain assignment).
my @array[2;2] = (-1, 1; 1, -1);
is-deeply @array.shape, (2, 2), 'shaped declaration with an initializer';
my @blank[2;3];
is-deeply @blank.shape, (2, 3), 'shaped declaration without one';

# `.grep` sees the leaves, and leaves the array's shape alone.
is-deeply @array.grep(* ≠ -∞).List, (-1, 1, 1, -1), 'grep over the leaves';
is-deeply @array.grep(* > 0, :k).List, (1, 2), ':k indexes the leaves';
is-deeply @array.shape, (2, 2), 'grep does not reshape the array';

# Reassigning a multi-dimensional shaped array fills it row by row.
@array = (-1, Inf; Inf, -1);
is-deeply @array.shape, (2, 2), 'reassignment keeps the shape';
is @array[0;1], Inf, '... and stores the rows';

# A Range row is that row's values.
my @row[1;3] = [1..3,];
is-deeply @row.shape, (1, 3), 'a Range row';
is @row[0;2], 3, '... is spread over the row';

constant $marker = 1;  # a BEGIN-time statement in this unit
