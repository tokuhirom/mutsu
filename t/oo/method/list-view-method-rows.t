use Test;

plan 12;

my $list = (10, 20);
my @array = 10, 20;

is-deeply $list.values.List, (10, 20).List, 'List.values is the positional view';
is-deeply @array.values.List, (10, 20).List, 'Array inherits List.values';
is-deeply $list.kv.List, (0, 10, 1, 20).List, 'List.kv interleaves indices and values';
is-deeply @array.kv.List, (0, 10, 1, 20).List, 'Array.kv preserves positional indices';
is-deeply $list.pairs>>.raku.List, ('0 => 10', '1 => 20').List,
    'List.pairs returns positional Pairs';
is-deeply @array.pairs>>.raku.List, ('0 => 10', '1 => 20').List,
    'Array.pairs returns positional Pairs';
is-deeply $list.antipairs>>.raku.List, ('10 => 0', '20 => 1').List,
    'List.antipairs reverses each positional Pair';
is-deeply @array.antipairs>>.raku.List, ('10 => 0', '20 => 1').List,
    'Array.antipairs reverses each positional Pair';

is-deeply (^3).map({ $list.values.elems }).List, (2, 2, 2).List,
    'List.values is stable at a repeated call site';
is-deeply (^3).map({ @array.kv.elems }).List, (4, 4, 4).List,
    'Array.kv is stable at a repeated call site';

my @mutable = 1, 2;
is-deeply @mutable.values.List, (1, 2).List,
    'values does not expose the backing Array cells';
is-deeply @mutable.pairs.map(*.key).List, (0, 1).List,
    'pairs keeps the Array indices';
