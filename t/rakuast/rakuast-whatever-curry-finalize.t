use v6;
use Test;

plan 7;

# A parser-planted WhateverCode priming scope is carried through the executing
# finalization (ADR-15760 slice 2) and curries exactly the same operands.

is (* + 1)(2), 3, 'a whatever operand primes the binary operator';
is (* + *)(3, 4), 7, 'two whatevers take two arguments';
is (*.uc)("a"), 'A', 'a method call on whatever';
is (1, 2, 3).map(* * 2).join(","), '2,4,6', 'a whatever as a map callback';
my @a = 1..5;
is @a[*-1], 5, 'a whatever subscript';
is (1..10).grep(* %% 2).join(","), '2,4,6,8,10', 'a whatever in a grep';
sub f { * ~ "x" }
is f()("q"), 'qx', 'a whatever returned from a routine';
