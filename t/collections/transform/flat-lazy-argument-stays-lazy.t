use Test;

# `flat` with several arguments, one of them an infinite lazy list, returns a
# lazy Seq instead of forcing the list (hang / "Cannot coerce an infinite lazy
# list to a strict list"). Found with Math::Sequences:
# `lazy flat 1, [\*] @primes`. #15328

plan 5;

is-deeply (flat 1, [\*] 1..*)[^3].List, (1, 1, 2), 'a scan as a later argument stays lazy';
is-deeply (flat 1, (1..*).map(* + 1), 7)[^4].List, (1, 2, 3, 4), 'a map pipe among plain arguments';

my @p = lazy (1..*).grep: { .is-prime };
my @a = lazy flat 1, [\*] @p;
is-deeply @a[^4].List, (1, 2, 6, 30), 'primorials over an infinite lazy array';

is-deeply (flat 0, (1..*).map({ ($_, [$_ + 100]) }))[^5].List, (0, 1, 101, 2, 102),
    'elements of a lazy part are flattened too';

is-deeply flat(1, (2, 3), [4, [5]]).List, (1, 2, 3, 4, [5]), 'finite arguments flatten as before';
