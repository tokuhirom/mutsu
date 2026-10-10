use Test;

plan 9;

# .Bool / .so / .not on a gather Seq pull one element, like the
# boolean-context forms (`so $e`, `?$e`).
my $empty = gather { if 0 { take 1 } };
is $empty.so,   False, 'empty gather Seq .so is False';
is $empty.Bool, False, 'empty gather Seq .Bool is False';
is $empty.not,  True,  'empty gather Seq .not is True';

my $full = gather { take 1; take 2 };
is $full.so,   True,  'non-empty gather Seq .so is True';
is $full.Bool, True,  'non-empty gather Seq .Bool is True';
is $full.not,  False, 'non-empty gather Seq .not is False';

# Only one element is pulled; the Seq keeps the rest.
my $seen = 0;
my $counted = gather { $seen++; take 1; $seen++; take 2 };
$counted.Bool;
is $seen, 1, '.Bool pulls exactly one element';
is $counted.list.elems, 2, 'the Seq still yields all elements afterwards';

is (so $empty), False, 'prefix so on an empty gather Seq stays False';
