use Test;

# From Iter::Able (t/16-insert-at): `X andthen .iterator but role {...}`. The
# right side is a value (a mixin of a non-code object), not a callable.
plan 5;

is (7 andthen 8 but 5), 8, 'a mixin on the right of andthen is its value';
is (7 andthen $_ + 1), 8, 'topic still reaches the right side';
my $r = ((1, 2) andthen .elems but role { method foo { 'foo' } });
is $r, 2, 'the mixin value is returned';
is $r.foo, 'foo', 'and carries the role';
my &f = -> $x { $x * 3 };
is (4 andthen &f), 12, 'a code object on the right is still called with the topic';
