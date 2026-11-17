use Test;

# Parentheses around an assignment end its list: in `my @a = (@b = 1), 2` the
# `, 2` belongs to `@a`'s list, not `@b`'s.

plan 6;

my @b;
my @a = (@b = 1), 2;
is-deeply @b, [1], 'the inner list assignment stops at its parenthesis';
is @a.elems, 2, 'the outer list takes the group and the rest';
is-deeply @a[1], 2, 'the rest is the outer list\'s';

my @c;
my @d;
@c = (@d = 1, 2);
is-deeply @d, [1, 2], 'a whole list inside the parentheses still goes to the inner array';
is-deeply @c, [1, 2], 'and the outer gets its value';

my ($p, $q);
$p = ($q = 5) + 1;
is "$p $q", '6 5', 'a parenthesized assignment followed by an operator';
