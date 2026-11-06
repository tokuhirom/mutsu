use Test;

plan 6;

# `postcircumfix:<[; ]>` is a CORE routine: callable by name and as a term.
my @a = [1, [2, 3]], [4, 5, 6];
is postcircumfix:<[; ]>(@a, (1, 0)), 4, 'indexes one dimension per list element';
is &postcircumfix:<[; ]>(@a, (0, 0)), 1, 'also through the code term';
is-deeply &postcircumfix:<[; ]>(@a, (*, *)).List, (1, [2, 3], 4, 5, 6), 'a * dimension takes every element';
my $s = ((1, (2, 3)), (4, 5, 6), 7);
is-deeply &postcircumfix:<[; ]>($s, (* for ^2)).List, (1, (2, 3), 4, 5, 6, 7), 'a list of Whatevers flattens a list one level';
is-deeply @a[*;*].List, (1, [2, 3], 4, 5, 6), 'same as the syntax';
is-deeply &postcircumfix:<[; ]>(@a, (1, 2)), 6, 'second row, third element';
