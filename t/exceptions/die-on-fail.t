use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 4;

my $Die-Off = 'BEGIN %*ENV<RAKU_TEST_DIE_ON_FAIL> = 0;';
my $Die-On  = 'BEGIN %*ENV<RAKU_TEST_DIE_ON_FAIL> = 1;';

is_run $Die-Off ~ 'use Test; plan 3; ok 1; ok 0; ok 1;', {
    :out("1..3\nok 1 - \nnot ok 2 - \nok 3 - \n"),
    :err(/:i 'failed' .+ 'line 1' .+ 'failed 1 test of 3'/),
    :1status,
}, 'failures do not stop the suite when RAKU_TEST_DIE_ON_FAIL is disabled';

is_run $Die-On ~ 'use Test; plan 3; ok 1; ok 0; ok 1;', {
    :out("1..3\nok 1 - \nnot ok 2 - \n"),
    :err(/:i
        'Stopping test suite' .+ 'RAKU_TEST_DIE_ON_FAIL' .+
        'planned 3 tests, but ran 2' .+ 'failed 1 test of 2'
    /),
    :255status,
}, 'failures stop the suite when RAKU_TEST_DIE_ON_FAIL is enabled';

is_run $Die-On ~ 'use Test; plan 1; todo "foo"; ok 0;', {
    :0status,
}, 'TODO failures do not trigger RAKU_TEST_DIE_ON_FAIL';

is_run $Die-On ~ 'use Test; plan 2; subtest "bar", { ok 0 }; ok 1;', {
    :err(/:i 'failed' .+ 'Stopping test suite'/),
    :255status,
}, 'subtest failures also trigger RAKU_TEST_DIE_ON_FAIL';
