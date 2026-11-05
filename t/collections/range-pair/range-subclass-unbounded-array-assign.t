use Test;

plan 6;

# `@`-assignment of an unbounded Range subclass instance stays a lazy array
# over the range, like a bare `0..*` (#15480).
class R is Range { }

my \r = R.new(0, Inf);
my @a = r;
is-deeply @a[^3].List, (0, 1, 2), 'an unbounded subclass instance reifies on demand';
is @a[5], 5, 'and further elements too';

my \f = R.new(0, 3);
my @b = f;
is-deeply @b.List, (0, 1, 2, 3), 'a finite subclass instance still fills the array';

my @c = 0..*;
is-deeply @c[^3].List, (0, 1, 2), 'a bare unbounded range is unchanged';

class T is Range { method iterator { (4, 5).iterator } }
my @d = T.new(0, 3);
is-deeply @d.List, (4, 5), 'a class with its own iterator keeps it';
my @e = T.new(0, Inf);
is-deeply @e[^2].List, (4, 5), 'also over an unbounded range';
