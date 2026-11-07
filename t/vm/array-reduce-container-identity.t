# `@array.reduce(&[=:=])` compares the elements' own Scalar containers: an
# Array's elements are fresh containers, so equal values are not identical (#16948).
use Test;
plan 6;

my ($x, $y) = 3, 3;
is [$x, $y].reduce(&[=:=]), False, 'two elements holding equal values are distinct containers';
is [$x, $x].reduce(&[=:=]), False, 'an Array copies its inputs into fresh containers';
is [$x, $y].reduce(&infix:<=:=>), False, 'the &infix:<=:=> spelling';
is [$x, $y].reduce(&[!=:=]), True, 'negated, distinct containers';

my @a = 1, 1, 1;
is @a.reduce(&[=:=]), False, 'array elements are distinct';
is (1, 1).reduce(&[=:=]), True, 'a List of equal immutable values is value-identical';
