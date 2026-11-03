use Test;
# From PDF::Grammar (COS::Actions `xref-section`): a shaped array may be
# initialized from a scalar variable holding the list of rows.
plan 5;

my $r = ([1,2],[3,4]).List;
my @a[2;2] = $r;
is @a.shape.gist, '(2 2)', 'shape kept';
is @a[1;0], 3, 'rows copied from a List in a scalar';

my $rows = (array[uint64].new(1,2,3,4), array[uint64].new(5,6,7,8)).List;
my uint64 @e[+$rows; 4] = $rows;
is @e[1;2], 7, 'native shaped array from a scalar holding native rows';

my $arr = [[1,2],[3,4]];
my @b[2;2] = $arr;
is @b[0;1], 2, 'itemized Array of rows';

throws-like { my @c[2;2] = 1, 2, 3, 4 }, X::Assignment::ToShaped, 'flat list still rejected';
