use Test;

plan 4;

is any(1, 2).map(* + 1).raku, '(any(2, 3),).Seq',
    'map treats a Junction as one item and returns its mapped Junction';

try { (1, 2).map() };
isa-ok $!, X::Cannot::Map, 'map without a mapper throws X::Cannot::Map';
is $!.what, 'List', 'X::Cannot::Map reports the invocant type';
is $!.using, "''", 'X::Cannot::Map reports the absent mapper';
