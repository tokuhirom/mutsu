use v6;
use lib 't/lib';
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 8;

# Regression for todo/deep/vendor-real-test-module.md: `roast/S32-list/skip.t`
# selectively imports Test's `plan`/`is`/etc. as `&`-sigil VALUES out of a `do`
# block, precisely so the core `skip` routine stays visible under its own bare
# name (Test also exports a `skip` sub that would otherwise shadow it):
#
#   BEGIN my (&plan, &is) = do {
#       use Test;
#       (&plan, &is)
#   }
#
# Under `MUTSU_REAL_TEST=1` (the real vendored Test.rakumod, not mutsu's
# native TAP provider) this died with "Unknown function: plan" -- even though
# `Test::plan` was still perfectly declared. Two independent bugs combined to
# cause it, both in `&`-sigil capture of a proto/multi routine reached only
# through an import that is lexically scoped to a block:
#
# Bug A (src/runtime/accessors_resolve.rs, resolve_code_var): capturing
# `&plan` as a value built a LAZY `Value::routine_parts(current_package,
# name)` reference that re-resolves "plan" BY NAME at call time. That name
# only resolved via the *importing* package's alias (`GLOBAL::plan`), which
# `pop_import_scope` correctly removes once the `do {}` block ends (an import
# is lexically scoped to its block) -- leaving the captured reference
# dangling. Fixed by materializing the actual multi-candidate bodies BY VALUE
# at capture time (same mechanism the sibling non-proto'd `is_multi` branch
# already used), so the resulting Sub keeps working regardless of what the
# registry does afterward.
#
# Bug B (src/runtime/run_prelude.rs, begin_body_is_hoistable): a top-level
# `BEGIN` whose body has no top-level declaration/bareword/call is pre-run at
# compile time via a separate `eval_block_value` sub-interpreter, which
# deliberately does NOT persist `&`-callable writes into the shared `env`
# (only plain lexicals). `begin_body_is_hoistable` only checked for a `use`
# statement at the BEGIN's own top level, missing one nested inside a `do {}`
# -- so `BEGIN my &plan = do { use Test; &plan }` (single-variable binding)
# was wrongly hoisted, and the captured value never reached the mainline. A
# BEGIN with a LIST-destructured binding (`BEGIN my (&plan, &is) = do {...}`)
# happened to dodge this by accident (its desugared AST contains "Call" in
# debug form, which already disqualified hoisting) -- so this bug was masked
# for the exact shape roast/S32-list/skip.t uses, and only surfaced once Bug A
# was isolated with a single-variable reduction. Fixed by also disqualifying
# hoisting when a `use` appears anywhere in the BEGIN body, not just at the
# top level.
#
# This file exercises both bugs directly with a LOCAL proto+multi module
# (t/lib/ProtoMultiCapture.rakumod) -- which reproduces under the DEFAULT
# native-provider mutsu too, since a user-defined routine (unlike `plan`/`is`)
# is never a hardcoded builtin name -- across all four combinations of
# BEGIN/no-BEGIN and single-variable/list-destructured binding. It then pins
# the original Test.rakumod scenario directly, toggling
# `MUTSU_REAL_TEST` via `is_run` so the real regression (only visible under
# the vendored Test module) is covered too, without changing what `t/`
# normally exercises.

# --- Bug A: no BEGIN, single-variable binding ---
{
    my &f = do {
        use ProtoMultiCapture;
        &proto-multi-capture
    }
    is-deeply (f(1), f("x")), ("int:1", "str:x"),
        'no BEGIN, single var: captured proto/multi still dispatches after the import scope pops';
}

# --- Bug B: BEGIN, single-variable binding ---
{
    BEGIN my &f = do {
        use ProtoMultiCapture;
        &proto-multi-capture
    }
    is-deeply (f(1), f("x")), ("int:1", "str:x"),
        'BEGIN, single var: captured proto/multi still dispatches (was wrongly hoisted)';
}

# --- no BEGIN, list-destructured binding ---
{
    my (&f) = do {
        use ProtoMultiCapture;
        (&proto-multi-capture)
    }
    is-deeply (f(1), f("x")), ("int:1", "str:x"),
        'no BEGIN, list binding: captured proto/multi still dispatches after the import scope pops';
}

# --- BEGIN, list-destructured binding (roast/S32-list/skip.t's own shape) ---
{
    BEGIN my (&f) = do {
        use ProtoMultiCapture;
        (&proto-multi-capture)
    }
    is-deeply (f(1), f("x")), ("int:1", "str:x"),
        'BEGIN, list binding: captured proto/multi still dispatches after the import scope pops';
}

# The selective import must stay selective: the bare name is not visible
# outside the `do {}` block that imported it. A direct bareword call to an
# undeclared routine is a COMPILE-time error in Raku (not caught by plain
# `try`), so the probe goes through EVAL — and EVAL itself still surfaces a
# compile error as a thrown exception rather than a `Failure`, so wrap that
# in `try` too.
ok !(try { EVAL('proto-multi-capture(1)'); True }),
    'the bare routine name did not leak outside its selective-import do block';

# --- The original roast/S32-list/skip.t shape, against the vendored
# Test.rakumod (MUTSU_REAL_TEST=1) as well as the native provider. ---
my $begin_list_probe = q:to/CODE/;
BEGIN my (&plan, &is) = do {
    use Test;
    (&plan, &is)
}
plan 1;
is 1, 1, 'selective import survived';
CODE

my $begin_single_probe = q:to/CODE/;
BEGIN my &plan = do {
    use Test;
    &plan
}
plan 1;
say "single var ok";
CODE

%*ENV<MUTSU_REAL_TEST>:delete;
is_run $begin_list_probe, { status => 0, out => "1..1\nok 1 - selective import survived\n" },
    'native provider: BEGIN + list-destructured selective import of Test still works';

%*ENV<MUTSU_REAL_TEST> = '1';
is_run $begin_list_probe, { status => 0, out => "1..1\nok 1 - selective import survived\n" },
    'vendored Test.rakumod: BEGIN + list-destructured selective import survives the import scope popping';
is_run $begin_single_probe,
    { status => 255, out => "1..1\nsingle var ok\n# You planned 1 test, but ran 0\n" },
    'vendored Test.rakumod: BEGIN + single-variable selective import survives (was wrongly hoisted)';

%*ENV<MUTSU_REAL_TEST>:delete;
