use Test;

plan 9;

is (1, 2, 3).iterator.count-only, 3, 'list iterator count-only works on temporary value';
is [1, 2, 3].iterator.count-only, 3, 'array iterator count-only works on temporary value';
is (1 .. 5).iterator.count-only, 5, 'range iterator count-only works on temporary value';

my $list-iter = (10, 20, 30).iterator;
is $list-iter.can('count-only').elems, 1, 'list iterator advertises count-only support';
is $list-iter.can('bool-only').elems, 1, 'list iterator advertises bool-only support';
is $list-iter.count-only, 3, 'count-only does not consume items';
is $list-iter.pull-one, 10, 'first pull after count-only still returns first item';
ok $list-iter.bool-only, 'bool-only is true while items remain';
is $list-iter.count-only, 2, 'count-only reflects remaining items after pull-one';
