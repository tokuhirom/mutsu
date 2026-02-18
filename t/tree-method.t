use Test;
plan 13;

# .tree on arrays
is (1, 2, (3, 4)).tree.elems, 3, '.tree preserves nested structure';
is (1, 2, (3, 4)).tree.join('|'), '1|2|3 4', '.tree join';

# .tree on non-iterables
cmp-ok 42.tree, '===', 42, '.tree on Int returns self';

# .tree on type objects
ok List.tree === List, '.tree on type object';

# .tree with depth
is (1, 2, (3, 4)).tree(1).join('|'), '1|2|3 4', '.tree(1)';
is (1, (2, (3, 4))).tree(2).[1].elems, 2, '.tree(2) two levels deep';
cmp-ok 42.tree(0), '===', 42, '.tree(0) on non-iterable';
cmp-ok 42.tree(*), '===', 42, '.tree(*) on non-iterable';

# .tree with WhateverCode closures
is ((1, 2), (3, 4)).tree(*.flat.join('|')), '1|2|3|4', 'WhateverCode form';
is ((1, 2), (3, 4)).tree(*.join(' '), *.join('|')), '1|2 3|4', 'multi-closure';

# zen slice
my @a = 1, 2, 3;
is @a[].join(','), '1,2,3', 'zen slice returns self';

# bare Z operator
is (<a b c> Z <X Y Z>).elems, 3, 'bare Z zips lists';
is (<a b c> Z <1 2 3>).join(' '), 'a 1 b 2 c 3', 'bare Z output';
