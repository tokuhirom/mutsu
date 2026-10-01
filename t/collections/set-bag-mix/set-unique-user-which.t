use Test;

plan 9;

class V { has $.n; method WHICH { ValueObjAt.new("V|$!n") } }
sub v($n) { V.new(:$n) }

is set(v(1), v(1)).elems, 1, 'set() keys elements by a user WHICH';
is bag(v(1), v(1)).elems, 1, 'bag() keys elements by a user WHICH';
is mix(v(1), v(1)).elems, 1, 'mix() keys elements by a user WHICH';
is (v(1), v(1)).Set.elems, 1, '.Set keys elements by a user WHICH';
is (v(1), v(1)).unique.elems, 1, '.unique compares by a user WHICH';
is unique(v(1), v(1)).elems, 1, 'unique() compares by a user WHICH';
is (v(1), v(1), v(2)).repeated.elems, 1, '.repeated compares by a user WHICH';
is (v(1), v(1), v(2)).squish.elems, 2, '.squish compares by a user WHICH';
is (v(1), v(2)).unique.elems, 2, 'distinct WHICH values stay distinct';
