use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

# Tests that run on a spawned thread (e.g. a Promise `.then` callback kept by
# another thread) bump the shared atomic TAP counter, not the subtest's local
# `ran` field. The subtest's emitted plan / count must reflect those threaded
# tests -- regression: a planless threaded subtest reported `1..0` instead of
# the true number of tests run, and a planned one would mis-count under
# enforcement (roast S17-promise/in.t).

plan 2;

# Planless subtest: finish emits `1..N` from the counted tests.
is_run q:to/CODE/,
    use Test;
    plan 1;
    subtest 'planless threaded' => {
        my $p = Promise.new;
        start { $p.keep(True); }
        await $p.then: { pass "a"; pass "b"; pass "c" };
    }
    CODE
    { :out(/'1..3'/ & /'ok 3 - c'/ & /'ok 1 - planless threaded'/), :0status },
    'planless subtest counts threaded passes (1..3)';

# Planned subtest matching the threaded count passes.
is_run q:to/CODE/,
    use Test;
    plan 1;
    subtest 'planned threaded' => {
        plan 2;
        my $p = Promise.new;
        start { $p.keep(True); }
        await $p.then: { pass "x"; pass "y" };
    }
    CODE
    { :out(/'ok 1 - planned threaded'/), :0status },
    'planned subtest with threaded passes matches its plan';
