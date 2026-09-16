use Test;

# infix:<∩> (and the other Set-level infix operators built on the same
# coercion) used to decontainerize a List-shaped ARRAY ELEMENT down to its
# own content when computing its Set-store key, while infix:<∈>/.grep's
# membership check (and the `.Set`/`.Bag`/`.Mix` method coercion) did not.
# Two distinct one-element Lists like `(5,)` therefore collided under `(&)`
# but stayed distinct under `(elem)`. `.WHICH` for a List/Array is
# per-object, so under real Raku all these operators agree that two
# separately-constructed `(5,)` Lists are DIFFERENT set members.
# https://github.com/tokuhirom/mutsu/issues/8570

plan 12;

my @b = ((1,), (2,), (5,));
my @c = ((5,), (4,));

is @c.grep(* (elem) @b).elems, 0,
    '(elem)/grep: two distinct (5,) Lists are not the same member';
is (@b (&) @c).elems, 0,
    '(&) intersect agrees with (elem): no shared List-identity member';
is (@b (|) @c).elems, 5,
    '(|) union keeps all 5 distinct List-identity members';
is (@b (-) @c).elems, 3,
    '(-) difference keeps all 3 of @b (nothing actually shared)';
is (@b (^) @c).elems, 5,
    '(^) symmetric difference keeps all 5 distinct members';

# Same shape with a real-Array element instead of a List -- itemization on
# array storage applies uniformly to both.
my @d = ([1], [2], [5]);
my @e = ([5], [4]);
is (@d (&) @e).elems, 0,
    '(&) intersect: two distinct [5] Arrays are not the same member either';

# Guard against under-fixing: plain scalar elements (no compound identity)
# must still dedupe by value as before.
my @p = 1, 2, 3;
my @q = 2, 3, 4;
is-deeply (@p (&) @q).keys.sort, (2, 3), '(&) still dedupes plain scalar elements';
is (@p (|) @q).elems, 4, '(|) still dedupes plain scalar elements';

# Guard against under-fixing the OTHER direction: a bare (non-itemized) List
# passed as a positional argument to the Set/Bag/Mix constructors still
# flattens fully, recursively -- this is unrelated array-element storage.
is Set(1, (2, 3), 4).elems, 4,
    'Set(1, (2,3), 4) still flattens a bare non-itemized List argument';
is Set(1, (2, (3, 4)), 5).elems, 5,
    'Set(...) still flattens nested bare Lists recursively';

# A genuine repeated List value (bound, not merely equal) is still ONE
# member -- the fix is about identity, not about Lists never colliding.
my $l = (9,);
my @r = ($l, $l);
is (@r (&) set($l)).elems, 1, 'a List bound twice is the same member (&)';

is (@b (&) @c).raku, 'set()',
    'intersect: empty, matching (elem)/grep';
