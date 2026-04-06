use Test;

plan 14;

# permutations function (sub form) with Int argument
is-deeply permutations(3),
    ((0, 1, 2), (0, 2, 1), (1, 0, 2), (1, 2, 0), (2, 0, 1), (2, 1, 0)).Seq,
    'permutations(3) returns permutations of 0..2';

# permutations with 1
is-deeply permutations(1), ((0,),).Seq, 'permutations(1) returns single element';

# permutations with 0
is-deeply permutations(0), ((),).Seq, 'permutations(0) returns empty tuple';

# permutations with negative
is-deeply permutations(-1), ((),).Seq, 'permutations(-1) returns empty tuple';

# permutations method form
is-deeply (1, 2, 3).permutations,
    ((1, 2, 3), (1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2), (3, 2, 1)).Seq,
    '.permutations method';

# single-element list
is-deeply (1,).permutations, ((1,),).Seq, 'single-element .permutations';

# empty list
is-deeply ().permutations, ((),).Seq, 'empty list .permutations';

# numeric coercion
is-deeply +().permutations, 1, '+().permutations is 1';
is-deeply +permutations(0), 1, '+permutations(0) is 1';

# permutations function with iterable argument
is-deeply permutations(<a b>).sort, <a b>.permutations.sort,
    'permutations with iterable matches method form';

# declaration binding in expression position
is-deeply +(my @i := <a b c>), 3,
    '+my @i := ... returns the declared array size';
is-deeply @i.permutations.sort, permutations(@i).sort,
    'bound array from expression position works with permutations';

is-deeply +permutations(30), 265252859812191058636308480000000,
    '+permutations(30) reports the total without materializing';
ok ?permutations(30),
    '?permutations(30) stays truthy without materializing';
