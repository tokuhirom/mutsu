use Test;

plan 9;

# A routine declared inside a block is compiled on the fly and dispatched
# through the name-keyed OTF caches. Those caches are keyed by the bare name
# and the callsite package, so re-declaring the same name in a sibling block
# must invalidate them -- otherwise the second block's calls would run the
# first block's body.

{
    sub f($x) { $x + 1 }
    is f(10), 11, 'block-local sub returns its own body (first block)';
    is f(10), 11, 'a repeated call to a block-local sub is stable';
}
{
    sub f($x) { $x + 100 }
    is f(10), 110, 'a sibling block re-declaring the name gets its own body';
}

# A file-scope sub shadowed by a block-local one of the same name: the outer
# body must come back once the block is left.
sub outer($x) { $x * 2 }
is outer(10), 20, 'file-scope sub before the shadowing block';
{
    sub outer($x) { $x * 3 }
    is outer(10), 30, 'block-local sub shadows the file-scope one';
}
is outer(10), 20, 'the file-scope sub is visible again after the block';

# The same shape reached through a closure argument -- what every
# `lives-ok { ... }` / `subtest { ... }` body looks like.
sub run-it(&body) { body() }
is run-it({ sub g($x) { $x - 1 }; g(5) }), 4, 'block-local sub inside a passed closure';
is run-it({ sub g($x) { $x - 2 }; g(5) }), 3, 'a second closure re-declaring it is not aliased';

# A block-local sub closing over the loop variable must observe each
# iteration's value, not the one captured on the first (cache-populating) call.
my @out;
for 1..3 -> $i {
    sub loopy($x) { $x + $i }
    @out.push(loopy(10));
}
is @out.join(','), '11,12,13', 'block-local sub sees the current loop binding';
