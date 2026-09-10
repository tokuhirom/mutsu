use Test;

# A hyper binary op recurses into nested Iterables at every depth. When an
# element is a Hash and the other operand is a scalar, the op distributes over
# the hash values, returning a Hash; nested Lists/Arrays recurse the same way.
# Mirrors the "hash in array" subtests in roast/S03-metaops/hyper.t.

plan 18;

my @a = (1, { a => 2, b => 3 }, 4);
my @b = <a b c>;
my @c = ('z', { a => 'y', b => 'x' }, 'w');
my @d = 'a' .. 'f';

# <<~>> : same-length, hash element distributes the scalar over its values
my @r = @a <<~>> @b;
is +@r, 3, '<<~>> result length';
is @r[0], '1a', '<<~>> scalar element';
is @r[1]<a>, '2b', '<<~>> hash element value a';
is @r[1]<b>, '3b', '<<~>> hash element value b';
is @r[2], '4c', '<<~>> scalar element';

# >>~<< : hash on both sides combines key-by-key
@r = @a >>~<< @c;
is @r[0], '1z', '>>~<< scalar';
is @r[1]<a>, '2y', '>>~<< hash-hash value a';
is @r[1]<b>, '3x', '>>~<< hash-hash value b';
is @r[2], '4w', '>>~<< scalar';

# <<~>> with dwim length extension on the right
@r = @a <<~>> @d;
is +@r, 6, '<<~>> dwim extends to longer side';
is @r[3], '1d', '<<~>> cycled scalar';
is @r[4]<a>, '2e', '<<~>> cycled hash value';
is @r[5], '4f', '<<~>> cycled scalar';

# unicode guillemet form behaves the same
@r = @a «~» @b;
is @r[1]<a>, '2b', '«~» hash element value a';
is @r[2], '4c', '«~» scalar element';

# nested plain arrays still recurse (no hash involved)
my @x = ((1, 2), (3, 4));
my @y = ((10, 20), (30, 40));
is (@x >>+<< @y).gist, '[(11 22) (33 44)]', 'nested arrays recurse element-wise';

# scalar broadcast into a nested array element
is ((1, (2, 3)) >>+>> 10).gist, '(11 (12 13))', 'scalar broadcasts into nested array';

# plain scalar op is unchanged
is (3 + 4), 7, 'scalar-scalar arithmetic unaffected';
