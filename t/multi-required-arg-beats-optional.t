use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

BEGIN %*ENV<MUTSU_REAL_TEST> = '1';

plan 1;

is_run 'use Test; is-approx 5, 6, 1, "within absolute tolerance";',
    {
        :out(/'ok 1 - within absolute tolerance'/),
        :err(''),
        :0status,
    },
    'the required tolerance overload beats the optional description overload';
