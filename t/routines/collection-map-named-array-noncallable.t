use Test;

plan 1;

my @a = 1, 2;
try @a.map(5).list;
isa-ok $!, X::Cannot::Map,
    'map on a named array rejects a non-callable with X::Cannot::Map';
