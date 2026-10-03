use Test;

# A native `array[T]` is not a List (its MRO is `array, Cool, Any, Mu`), so
# `(List:D $a)` does not take one and `(array:D $a)` does. The multi-dispatch
# cache used to key a native array exactly like an `Array`, so a call that had
# first dispatched an `Array` to `(List:D $a)` sent the next native array
# there too, and it died binding (PDF::Grammar::Test's `json-eqv`).

plan 9;

my uint64 @flat = 1, 2;
my uint64 @shaped[1; 4];

nok @flat ~~ List, 'a native array is not a List';
nok @shaped ~~ List, 'nor is a shaped one';
ok @flat ~~ array, 'it is an array';
ok @flat ~~ Positional, 'and Positional';
ok [1, 2] ~~ List, 'an Array is still a List';

multi j(List:D $a, $b) { 'list' }
multi j(array:D $a, $b) { 'array' }
multi j(Mu $a, Mu $b) { 'mu' }

is j([1, 2], 0), 'list', 'an Array picks (List:D)';
is j(@shaped, 0), 'array', 'a native array after it picks (array:D)';
is j(@flat, 0), 'array', 'so does an unshaped one';
is j([3], 0), 'list', 'and an Array again picks (List:D)';
