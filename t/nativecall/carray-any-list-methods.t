use Test;
use NativeCall;

# CArray declares its own `list`, so Any's `self.list` methods answer from it
# (CSS::Properties' `t/declarations.t`: `.edges.Array`).
plan 12;

my $c = CArray[uint16].new(1, 2, 3);
is $c.Array.raku, '[1, 2, 3]', '.Array';
is $c.List.raku, '(1, 2, 3)', '.List';
is $c.join('-'), '1-2-3', '.join';
is $c.elems, 3, '.elems';
is $c.cache.raku, '(1, 2, 3)', '.cache';
is $c.eager.raku, '(1, 2, 3)', '.eager';
is $c.kv.join(','), '0,1,1,2,2,3', '.kv';
is $c.pairs.map(*.key).join(','), '0,1,2', '.pairs';
is $c.keys.join(','), '0,1,2', '.keys';
is $c.flat.join(','), '1,2,3', '.flat';
is $c.reverse.join(','), '3,2,1', '.reverse';
is $c.end, 2, '.end';
