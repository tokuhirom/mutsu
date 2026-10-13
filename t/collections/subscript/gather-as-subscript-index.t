use Test;

plan 4;

# A plain `gather` used as a subscript index is pulled first (#13069).
is-deeply <a b c>[gather { take 1; take 2 }].List, ('b', 'c'), 'gather index on a list literal';

my @a = <a b c>;
is-deeply @a[gather { take 1; take 2 }].List, ('b', 'c'), 'gather index on an array';
is-deeply @a[gather { take 0 }].List, ('a',), 'single-element gather index';
is-deeply @a[gather { take 2; take 0 }].List, ('c', 'a'), 'order of taken indices is kept';
