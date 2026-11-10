use Test;

# `hash(+@list)` flattens the plain Lists among its elements; itemized lists
# and Arrays stay whole. Identical under rakudo.
plan 8;

is-deeply hash((1,2),(3,4)), {"1" => 2, "3" => 4}, 'several List arguments';
is-deeply hash(((1,2),(3,4))), {"1" => 2, "3" => 4}, 'one List of Lists';
is-deeply hash(((1,2),(3,4)).Seq), {"1" => 2, "3" => 4}, 'one Seq of Lists';
is-deeply hash((1,2) Z (3,4)), {"1" => 3, "2" => 4}, 'a zip';
is-deeply hash(('A' .. *) Z <x y z>), {A => "x", B => "y", C => "z"}, 'a zip with a lazy range';
my @a = (1,2),(3,4);
is-deeply hash(@a), {"1 2" => $(3, 4)}, 'an Array keeps its itemized Lists whole';
is-deeply hash(1,2,3,4), {"1" => 2, "3" => 4}, 'plain scalars';
is-deeply hash(<A B C> Z=> 1..3), {A => 1, B => 2, C => 3}, 'pairs';
