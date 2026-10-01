use Test;

# `@a[$i, $r] = @a[$r, $i]` on a plain Array of Arrays swaps rows. The flat
# slice-assignment path wrongly demanded one index per nesting level (X::NotEnough
# Dimensions) because it measured the nested depth of an *unshaped* array.
# Found via Math::Matrix's reduced-row-echelon-form.

plan 4;

my @a = [1,2],[3,4],[5,6];
@a[0, 2] = @a[2, 0];
is-deeply @a, [[5,6],[3,4],[1,2]], 'swap first and last rows by slice';

my ($i, $r) = 0, 1;
@a[$i, $r] = @a[$r, $i];
is-deeply @a, [[3,4],[5,6],[1,2]], 'swap with variable indices';

my @b = [1,2],[3,4];
@b[0, 1] = [9,9], [8,8];
is-deeply @b, [[9,9],[8,8]], 'assign fresh rows by slice';

my @s[2;2] = [1,2],[3,4];
throws-like { @s[1] = 5 }, X::NotEnoughDimensions, 'shaped array still demands full dimensions';
