use Test;

plan 7;

# A shaped array (`my @a[2;2]`) has fixed dimensions. An out-of-range index in
# any dimension is an error (raku throws), not a silent no-op yielding Any.

my @arr[2;2] = <a b>, <c d>;

dies-ok { @arr[2;0]:delete }, 'delete out of range in dimension 1 dies';
dies-ok { @arr[0;2]:delete }, 'delete out of range in dimension 2 dies';
dies-ok { @arr[5;5]:delete }, 'delete far out of range dies';
dies-ok { @arr[-3;0]:delete }, 'delete with too-negative index dies';

# In-range deletes still work and return the old value.
is @arr[0;0]:delete, 'a', 'in-range delete returns old value';
is @arr[1;1]:delete, 'd', 'second in-range delete returns old value';

# A negative index within range counts from the end and is allowed.
my @b[3] = <x y z>;
is @b[*-1]:delete, 'z', 'negative-in-range delete works';
