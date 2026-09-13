use Test;

# `classify(*)` / `categorize(*)` classify on the *identity* of each element
# (`multi method classify(Whatever)`; rakudo 2023.02). mutsu had no Whatever arm
# in the classifier-mapper match, so `*` fell into the catch-all and every
# element was keyed under `Nil` -- Math::NumberTheory's `factor-integer` uses
# `@factors.classify(*)` to count prime multiplicities and got one `(Nil, n)`
# pair back instead of one pair per distinct prime.

plan 10;

is-deeply (2, 2, 2, 3, 5).classify(*).sort(*.key).map({ $_.key ~ '=' ~ $_.value.elems }).join(','),
    '2=3,3=1,5=1',
    'classify(*) buckets by element identity';

is-deeply (2, 2, 3).categorize(*).sort(*.key).map({ $_.key ~ '=' ~ $_.value.elems }).join(','),
    '2=2,3=1',
    'categorize(*) buckets by element identity';

# The key is the element, the value is what `:as` (if any) made of it.
my %as = (1, 2, 3).classify(*, :as(* * 10));
is %as{1}[0], 10, 'classify(*, :as) keys on the element';
is %as{3}[0], 30, 'classify(*, :as) stores the mapped value';

# A Str element keys on itself, like any other.
my %s = <a b a>.classify(*);
is %s<a>.elems, 2, 'classify(*) on strings groups repeats';
is %s<b>.elems, 1, 'classify(*) on strings keeps singletons apart';

# A Range receiver is classified element-wise.
is (1..4).classify(*).keys.elems, 4, 'classify(*) over a Range gives one bucket per element';

# Identity is exactly a `{ $_ }` mapper, so a list-valued element is a
# multi-level path for classify (mixed levels are an error) and several
# categories for categorize.
my %c = ((1, 2), (1, 2), (3, 4)).categorize(*);
is %c{1}.elems, 2, 'categorize(*) files a list element under each of its members';
is %c{3}.elems, 1, 'categorize(*) files the other list under its own members';

dies-ok { ((1, 2), (3,)).classify(*) },
    'classify(*) rejects mixed-level identity paths';
