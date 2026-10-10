use v6;
use lib 't/lib';
use Test;

# Found via the Color::Names ecosystem distribution: two `unit class` files that
# each declare a file-scope `my constant COLORS` must not share one binding.
plan 9;

use UnitConst::A;
use UnitConst::B;

is-deeply UnitConst::A.data.keys.sort.list, ('a',), 'first unit class keeps its constant';
is-deeply UnitConst::B.data.keys.sort.list, ('b', 'c'), 'second unit class keeps its own constant';
is UnitConst::A.plain, 'plain-a', 'plain constant, first unit class';
is UnitConst::B.plain, 'plain-b', 'plain constant, second unit class';
is DEFAULT, 'default-a', 'a default-exported constant still reaches the importer';
eval-dies-ok 'PLAIN', 'a unit class file-scope constant does not leak into the importer';
eval-dies-ok 'COLORS', 'a tag-exported constant is not imported without its tag';

for <A B> -> $s {
    my $p = "UnitConst::$s";
    require ::($p);
    is ::($p).data.elems, $s eq 'A' ?? 1 !! 2, "dynamic require of $s sees its own constant";
}
