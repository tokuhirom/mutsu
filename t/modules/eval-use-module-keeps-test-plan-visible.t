use Test;
use lib $*PROGRAM.parent(2).add('fixtures/eval-use-module/lib');

# #13473: Test's own routines (eval-lives-ok runs an EVAL inside Test's
# compunit) loading a module there re-scoped Test's `plan` family to Test
# alone, so the script that had imported it died with `Unknown function: plan`.
eval-lives-ok 'use EvalUsedModule', 'EVAL of a use inside Test code';

subtest 'plan is still callable from a subtest' => {
    plan 1;
    ok True, 'inside';
}

done-testing;
