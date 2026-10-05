use v6.e;
use Test;

plan 12;

my @array = 1, 2, 3;
is (1, 2, 3).list.^name, 'List', 'List.list keeps a positional List';
is (1, 2, 3).List.^name, 'List', 'List.List returns a List view';
is (1, 2, 3).Array.^name, 'Array', 'List.Array creates an Array';
ok (1, 2, 3).Array !=== (1, 2, 3), 'List.Array creates fresh storage';
ok @array.Array !=== @array, 'Array.Array creates fresh storage';
is @array.list.^name, 'Array', 'Array.list keeps the plain Array view';
is @array.List.^name, 'List', 'Array.List returns a List view';
is-deeply @array.List, (1, 2, 3), 'Array.List preserves elements';

my $itemized = $[1, 2, 3];
is $itemized.list.^name, 'Array', 'itemized Array.list keeps its positional view';
is $itemized.List.^name, 'List', 'itemized Array.List returns a List';
is-deeply $itemized.Array.List, (1, 2, 3), 'Array conversion preserves values';

is-deeply (1, 2, 3).list.List, (1, 2, 3), 'repeated positional conversions are stable';
