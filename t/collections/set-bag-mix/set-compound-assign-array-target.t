use Test;

plan 6;

# #12399: a Set/Bag/Mix assigned to an `@` variable lists its original
# element objects, not the internal `Int|1` key strings.
my @a = set(1);
is @a[0].key.WHAT.gist, '(Int)', 'my @a = set(1) keeps the Int key';
my @b = bag(1, 1);
is @b.raku, '[1 => 2]', 'my @b = bag(1, 1)';
my @m = (1 => 1.5).Mix;
is @m[0].key.WHAT.gist, '(Int)', 'Mix element key keeps its type';

my @c = set(1);
@c[0] (-)= set(1);
is @c[0].elems, 0, '@a[0] (-)= set(1) stores the difference';

my @d = (1, 2);
@d (+)= (3,);
is @d.map(*.key).sort.join(','), '1,2,3', '@a (+)= (3,) keeps Int keys';
is @d.map(*.value).sum, 3, '... with weights';
