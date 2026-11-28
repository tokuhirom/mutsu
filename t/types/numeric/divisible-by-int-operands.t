use Test;

# `%%` on two plain Ints answers through the shared floored modulus without
# coercing its operands (#19323); every other shape keeps the general path.

plan 11;

ok 6 %% 3, 'divisible';
nok 7 %% 3, 'not divisible';
nok -6 %% 4, 'a negative dividend';
ok -8 %% 4, 'a negative dividend that divides';
ok 7 %% -7, 'a negative divisor';
ok 9223372036854775807 %% -1, 'the largest Int by -1';
ok (-9223372036854775807 - 1) %% -1, 'the smallest Int by -1 does not overflow';
isa-ok 6 %% 0, Failure, 'a zero divisor is a soft Failure';
ok 6 !%% 0, '!%% of a zero divisor is True';
ok 10 %% 2.5, 'a Rat divisor';
ok 2**70 %% 2, 'a big Int dividend';
