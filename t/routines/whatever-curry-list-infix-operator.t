use Test;

plan 4;

# A user infix at list-infix precedence (`is equiv<Z>`) is looser than the
# comma, so a Whatever-curried `* op a, b` takes the whole list as its right
# operand.
sub infix:<count-rest> ($topic, *@rest) is equiv<Z> { @rest.elems }

is-deeply (1, 2).map(* count-rest 7, 8, 9), (3, 3), 'curried list infix in map takes the whole list';
is (* count-rest 1, 2)(0), 2, 'curried list infix in parens takes the whole list';
my @r = (1, 2).map: * count-rest 7, 8;
is-deeply @r, [2, 2], 'colon-call argument keeps the whole list';
is (5 count-rest 1, 2), 2, 'uncurried form still takes the whole list';
