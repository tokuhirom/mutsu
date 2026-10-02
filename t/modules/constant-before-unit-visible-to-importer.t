use v6.d;
use Test;
use lib $?FILE.IO.parent(2).add('lib');

# Distribution: Dist::META (t/00-sanity.t reads its file-scope `constant %phases-eq`).
plan 6;

use ConstBeforeUnitClass;

is %before-hash<b>, 2, 'hash constant before `unit class` is visible to the importer';
is $before-scalar, 5, 'scalar constant before `unit class` is visible';
is @before-list.elems, 3, 'array constant before `unit class` is visible';
is ConstBeforeUnitClass.after, 9, 'constant after `unit class` still works inside the unit';
is ConstBeforeUnitClass.before, 9, 'the unit reads its own pre-unit constants';
is (try EVAL('%before-hash<a>')), 1, 'visible through EVAL too';
