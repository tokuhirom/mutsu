use Test;

plan 8;

is((1,).kv, [0, 1], 'list .kv returns positional index/value');
is(42.kv, [0, 42], 'scalar .kv returns [0, value]');
is(kv(42), [0, 42], 'kv(scalar) delegates to .kv');
is(keys(42), [0], 'keys(scalar) returns index 0');
is(values(42), [42], 'values(scalar) returns the scalar value');
is(pairs(42).[0].kv, [0, 42], 'pairs(scalar) returns positional pair');
is(+Any.kv, 0, 'type object .kv is empty');
is(+kv(Any), 0, 'kv(type object) is empty');
