use Test;

# Regression: a plain assignment (`$r = 0`) to an `atomicint` scalar performed
# from inside a *nested closure frame* (e.g. a `subtest {...}` block) must reset
# the shared atomic cell, so a subsequent atomic op re-seeds from the assigned
# value instead of reading the stale shared value.
#
# Previously the reset only fired on the SetLocal store path and only when the
# atomic name->value_key mapping lived in the current frame's env, so the reset
# silently no-op'd from a nested frame and the atomic value kept accumulating.
# (roast S03-metaops/hyper.t test 408.)

plan 5;

# --- bare block (same frame): always worked, kept as a guard ---
{
    my atomicint $r = 0;
    sub bump-a is nodal { $r⚛++; 1 }
    bump-a(); bump-a();
    $r = 0;
    bump-a();
    is $r, 1, 'bare-block: reset then one atomic increment gives 1';
}

# --- reset inside a subtest closure, atomic op in an outer sub ---
{
    my atomicint $runs = 0;
    sub bump-b is nodal { $runs⚛++; 1 }
    subtest 'first subtest resets $runs at the end' => {
        plan 1;
        bump-b(); bump-b();
        is $runs, 2, 'two increments';
        $runs = 0;
    }
    subtest 'second subtest sees the reset' => {
        plan 1;
        bump-b(); bump-b();
        is $runs, 2, 'reset from the previous closure took effect';
        $runs = 0;
    }
}

# --- atomic assign / atomic-fetch cross-frame coherence ---
{
    my atomicint $c = 10;
    my $block = { $c = 3; };
    $block();
    is atomic-fetch($c), 3, 'plain assign in a closure is visible to atomic-fetch';
    atomic-add-fetch($c, 4);
    is $c, 7, 'atomic-add after a closure reset re-seeds from the assigned value';
}
