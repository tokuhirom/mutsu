use Test;

plan 6;

# `&[==]` called with containers (what an `is raw` routine returns) compares
# their values (#17337).

my sub id(\value) is raw { value }
my @a = 1, 1, 2, 2.5e0, 2.5e0;
my &eq = &[==];

ok eq(id(@a[0]), id(@a[1])), 'two Int containers with equal values';
nok eq(id(@a[1]), id(@a[2])), 'two Int containers with different values';
ok eq(id(@a[3]), id(@a[4])), 'two Num containers with equal values';
ok eq(id(@a[2]), 2), 'a container and a value';
is (1, 1, 2, 2, 1).squish(:as(&id), :with(&[==])).List, (1, 2, 1),
  'squish through an `is raw` :as and `==` :with';
is @a.squish(:with(&[==])).List, (1, 2, 2.5e0), 'squish over an array';
