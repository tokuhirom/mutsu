use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 4;

# A subtest that declares a plan must run exactly that many tests. Under-running
# the plan makes the subtest report `not ok` even when no inner test failed.
is_run 'use Test; plan 1; subtest "under", { plan 3; ok 1, "a"; };', {
    :out(/'# Subtest: under' .+ 'ok 1 - a' .+ 'planned 3 test' .+ 'but ran 1' .+ 'not ok 1 - under'/),
    :err(/:i 'failed 1 test of 1'/),
    :1status,
}, 'under-running a subtest plan fails the subtest';

# Over-running the plan also fails the subtest.
is_run 'use Test; plan 1; subtest "over", { plan 1; ok 1, "a"; ok 1, "b"; };', {
    :out(/'not ok 1 - over'/),
    :1status,
}, 'over-running a subtest plan fails the subtest';

# A subtest that runs exactly its planned count passes.
is_run 'use Test; plan 1; subtest "exact", { plan 2; ok 1, "a"; ok 1, "b"; };', {
    :out(/'ok 1 - exact'/),
    :0status,
}, 'matching the subtest plan passes';

# A planless subtest still passes when all inner tests pass.
is_run 'use Test; plan 1; subtest "planless", { ok 1, "a"; ok 1, "b"; };', {
    :out(/'ok 1 - planless'/),
    :0status,
}, 'planless subtest passes with all inner tests ok';
