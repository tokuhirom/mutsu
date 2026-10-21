use Test;

# A negative index never exists: EXISTS-POS answers False without reifying an
# infinite list.
plan 5;

is (1..Inf).map(* + 1).EXISTS-POS(-1), False, 'negative index on an infinite lazy Seq';
is (1..Inf).map(* + 1).EXISTS-POS(2), True, 'a non-negative index still pulls';
my @a = lazy 1..*;
is @a.EXISTS-POS(-1), False, 'a lazy array';
is @a.EXISTS-POS(3), True, 'a lazy array, non-negative';
is (1, 2, 3).Seq.EXISTS-POS(-1), False, 'a finite Seq';
