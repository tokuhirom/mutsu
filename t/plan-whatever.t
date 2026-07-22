use Test;

# `plan *` (a Whatever plan) means the test count is unknown up front: no
# `1..N` header is emitted, and the run must not die with "plan expects Int".
# The trailing count comes from `done-testing` (exercised here), matching raku.

plan *;

ok 1, 'first assertion after plan *';
ok 2 == 2, 'second assertion';
is 3, 3, 'is works under plan *';
nok False, 'nok works under plan *';

done-testing;
