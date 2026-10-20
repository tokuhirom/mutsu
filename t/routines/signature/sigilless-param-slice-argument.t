use Test;

# A slice passed to a sigilless parameter binds the selected elements'
# containers, so assigning the parameter writes through to them (#13786).
# A scalar `is rw` parameter still refuses a slice, which is a List, not a
# container. Expected values are the `raku` (v2026.09) output.

plan 12;

sub d(\t) { t = (7, 8) }

my @a = 1, 2, 3;
d(@a[0, 2]);
is @a.raku, '[7, 2, 8]', 'an array slice is written through';

my %h = a => 1, b => 2, c => 3;
d(%h<a c>);
is %h<a b c>.join(','), '7,2,8', 'a hash slice is written through';

my %m = a => 1;
d(%m<a z>);
is %m.sort.map(*.kv.join('=')).join(','), 'a=7,z=8', 'a missing hash key is vivified by the write';

my @g = 1, 2, 3;
sub m(\t) { t = (9, 9) }
m(@g[0, 5]);
is @g.raku, '[9, 2, 3, Any, Any, 9]', 'an element past the end is vivified by the write';

my @r = 1;
sub rd(\t) { t.elems }
is rd(@r[0, 3]), 2, 'reading a slice parameter';
is @r.raku, '[1]', '... does not vivify the missing element';

my @e = 1, 2, 3;
sub e(\t) { t = () }
e(@e[()]);
is @e.raku, '[1, 2, 3]', 'an empty slice is a writable selection of nothing';

my @rg = 1, 2, 3;
d(@rg[1..2]);
is @rg.raku, '[1, 7, 8]', 'a range slice is written through';

my @dup = 1, 2, 3;
d(@dup[0, 0]);
is @dup.raku, '[8, 2, 3]', 'a repeated index takes the last write';

my @cv = 1, 2, 3;
my &cw = sub (\t) { t = (6, 6) };
cw(@cv[1, 2]);
is @cv.raku, '[1, 6, 6]', 'a slice passed to a sub held in a code variable';

sub g($x is rw) { $x = 5 }
throws-like { g(@a[0, 1]) }, X::Parameter::RW, 'a scalar `is rw` parameter refuses a slice';

sub n($x) { $x.elems }
is n(@a[0, 1]), 2, 'a plain parameter reads the slice';
