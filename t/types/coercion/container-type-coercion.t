# The container-type coercion calls `Array(...)`, `List(...)`, `Hash(...)`.
# Each argument becomes one element (rakudo does not deep-flatten these), so
# `Array(1,2,3)` is a 3-element Array. Previously mutsu reported these as
# "Unknown function: Array" (S02-types/array.t aborted on `Array(1,2,3)`).
use Test;

plan 10;

is Array(1, 2, 3).WHAT.gist, '(Array)', 'Array(...) makes an Array';
ok Array(1, 2, 3) eqv [1, 2, 3], 'Array(1,2,3) makes the correct array';
is Array(1, 2, 3).elems, 3, 'Array(...) keeps each arg as one element';
is Array((1, 2), 3).elems, 2, 'Array(...) does NOT deep-flatten a nested list arg';

is List(1, 2, 3).WHAT.gist, '(List)', 'List(...) makes a List';
ok List(1, 2, 3) eqv (1, 2, 3), 'List(1,2,3) makes the correct list';

is Hash('a', 1, 'b', 2).WHAT.gist, '(Hash)', 'Hash(...) makes a Hash';
is Hash('a', 1, 'b', 2)<a>, 1, 'Hash(...) pairs up args into entries';

# A lone type-object argument is a parametric type request, not a value list.
is Array(Int).gist, '(Array(Int))', 'Array(Int) renders as a parametric type';
is List(Str).gist, '(List(Str))', 'List(Str) renders as a parametric type';
