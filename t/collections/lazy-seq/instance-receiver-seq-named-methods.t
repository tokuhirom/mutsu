use Test;

# A user-class instance receiver skips the lazy-Slip / shaped-array / Seq
# pre-dispatch guards on `CallMethodMut`; methods named after the ones those
# guards act on must still reach the class's own methods.

plan 9;

class Boxed {
    has $.tag = 'b';
    method map($f)      { "map:$f" }
    method head($n = 1) { "head:$n" }
    method first()      { "first" }
    method Slip()       { "slip" }
    method lazy()       { "lazy" }
    method elems()      { 42 }
    method AT-POS($i)   { "at:$i" }
    method iterator()   { "iter" }
}

my $o = Boxed.new;
is $o.map(3),     'map:3',  'user map on an instance';
is $o.head(2),    'head:2', 'user head on an instance';
is $o.first,      'first',  'user first on an instance';
is $o.Slip,       'slip',   'user Slip on an instance';
is $o.lazy,       'lazy',   'user lazy on an instance';
is $o.elems,      42,       'user elems on an instance';
is $o.AT-POS(1),  'at:1',   'user AT-POS on an instance';
is $o.iterator,   'iter',   'user iterator on an instance';

# The same names on real Seq / shaped-array receivers still take the guards.
my @s = (1, 2, 3).map({ $_ * 2 });
is-deeply @s.head(2).list, (2, 4), 'Seq receiver keeps its pre-dispatch handling';
