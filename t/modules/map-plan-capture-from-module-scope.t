use v6;
use lib $*PROGRAM.parent(2).add('lib-exported-our-arrays');
use Test;
use ExportedOurArrays;

# An inline map/grep plan takes from its callback's capture only the names
# the body can reach. From a wide module scope it now selects them through a
# per-layer index instead of visiting every name (#19352); a name the body
# mentions -- by its bare spelling, its package-qualified one, as a type or as
# a routine -- must still be there.

plan 6;

class Point { has $.x }
constant Scale = 3;

is (1, 2).map({ Point.new(x => $_).x }).list, (1, 2), 'a map body names a type';
is (1, 2).map({ $_ * Scale }).list, (3, 6), 'a map body names a constant';
is (^3).grep({ @Squares[$_] > 1 }).list, (1, 2), 'a grep body names an exported array';
is (^2).map({ @ExportedOurArrays::Fib[$_ + 5] }).list, (5, 8), 'a map body names a package array';
is (1, 2).map({ square-sum($_) }).list, (1, 5), 'a map body calls an imported routine';
sub helper($v) { $v ~ '!' }
is <a b>.map({ helper($_) }).list, <a! b!>, 'a map body calls a local sub';
