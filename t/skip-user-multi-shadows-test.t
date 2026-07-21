use v6;
use Test;

# A user-imported list `skip` (e.g. from the head-skip-tail dist, which defines
# `my proto sub skip(Mu, |) {*}` + `my multi sub skip($n, +values)`) must win
# over Test's TAP `skip` directive once `plan` activates test mode. The
# interpreter's `exec_call` runs `call_test_function` before user-sub
# resolution, so the skip name needs the same list-vs-Test disambiguation there
# that the other dispatch sites already apply. Regression: the multi user `skip`
# was shadowed by Test's `skip`, which emitted "# SKIP N" and returned Nil.

my proto sub skip(Mu, |) {*}
my multi sub skip($skip, +values) { values.skip($skip) }

plan 5;

my @a = ^10;

# Numeric first arg.
is-deeply skip(4, @a), (4, 5, 6, 7, 8, 9), 'skip(Int, @list) uses the list routine, not Test::skip';

# WhateverCode first arg (`*-4`).
is-deeply skip(*-4, @a), (6, 7, 8, 9), 'skip(WhateverCode, @list) uses the list routine';

# A Range operand (list-like second arg) is also list-skip.
is-deeply skip(2, 1..5), (3, 4, 5), 'skip(Int, Range) uses the list routine';

# Test's own skip directive (Str reason) is unaffected — it still emits a TAP
# SKIP for the given count.
subtest 'Test::skip directive still works' => {
    plan 3;
    ok 1, 'a real test';
    skip 'deliberately skipped', 2;
}

pass 'reached the end without the skip routine consuming tests';
