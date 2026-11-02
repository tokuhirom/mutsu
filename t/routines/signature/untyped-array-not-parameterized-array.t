use Test;

plan 9;

# An untyped Array or List is not an `Array[Int]`, however well its elements
# fit; only a container declared with the element type is (#15336).
nok (1, 2, 3) ~~ Array[Int], 'a List does not match Array[Int]';
nok [1, 2] ~~ Array[Int], 'an untyped Array does not match Array[Int]';
my Int @a = 1, 2;
ok @a ~~ Array[Int], 'a typed Array does match';
ok Array[Int].new(1, 2) ~~ Array[Int], 'an Array[Int].new does match';

subset Ints of Array[Int];
is Ints((1, 2, 3)).raku, 'Array[Int].new(1, 2, 3)', 'a subset over Array[Int] coerces a List';
is Ints([1, 2]).raku, 'Array[Int].new(1, 2)', 'and an untyped Array';

sub f(Array[Int] $x) { $x.elems }
throws-like { f([1, 2]) }, X::TypeCheck::Binding::Parameter, 'binding an untyped Array fails';
is f(Array[Int].new(1, 2)), 2, 'binding a typed Array works';
nok [1, 2] ~~ Positional[Int], 'nor does an untyped Array satisfy Positional[Int]';
