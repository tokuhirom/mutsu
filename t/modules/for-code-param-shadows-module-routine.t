use Test;

# Humming-Bird (Humming-Bird::Core::Route.CALL-ME): `for @middlewares ->
# &middleware { &middleware($req, $res, ...) }` inside a module that also
# declares `multi sub middleware` called the module's multi family, not the
# loop's callable, when the callables came from the importing program.

plan 3;

use lib 't/lib';
use ForCodeParamModule;

is-deeply run-all([-> $a, $b { "two $a $b" }, -> $a, $b { "again $b" }]),
    ['two 7 r', 'again r'], 'a for-loop &param is called, not the same-named module multi';
is-deeply map-all([-> $x { "mapped $x" }]), ('mapped 8',),
    'a pointy-block &param of map is called, not the module multi';
is middleware([1]), 'array', 'the module multi family still works for its own callers';
