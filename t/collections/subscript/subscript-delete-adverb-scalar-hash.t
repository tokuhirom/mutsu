use Test;

# Found via the Cookie::Jar ecosystem distribution: `$h<a b>:delete:kv` on a
# scalar holding a Hash must remove the keys, not only report them.
plan 6;

my $h = {a => 1, b => 2, c => 3};
is-deeply ($h<a b>:delete:kv).list, ('a', 1, 'b', 2), ':delete:kv reports pairs';
is-deeply $h.keys.sort.list, ('c',), 'keys deleted from the scalar-held hash';

my $g = {a => 1, b => 2};
is-deeply ($g<a z>:delete:k).list, ('a',), ':delete:k reports existing keys';
is-deeply $g.keys.list, ('b',), 'key deleted';

my $v = {a => 1, b => 2};
is-deeply ($v<a>:delete:kv).list, ('a', 1), 'single key :delete:kv';
is-deeply $v.keys.list, ('b',), 'single key deleted';
