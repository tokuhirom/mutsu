use v6;
use Test;

# Regression test for a process-crashing bug
# (was todo/deep/recursive-sub-trailing-comma-array-literal-of-own-param-stack-overflow.md,
# fixed in news/2026-08/):
#
# A recursive sub that builds a `my @array = ($param,);` local (a
# parenthesized list literal with a trailing comma, forcing List context for
# a single element that is the routine's OWN parameter) crashed mutsu with a
# native Rust stack overflow (SIGABRT), even at recursion depth 1.
#
# Confirmed root cause (via `rust-gdb`, not just static reading): the
# trailing-comma list literal aliases the array element to the parameter's
# own container (`WrapVarRef` + `capture_var_cell_inner` in
# `src/vm/vm_data_ops.rs`), which boxes the parameter's local slot into a
# shared `ContainerRef` cell and mirrors it into the call's `env` overlay.
# `exec_get_local_op`'s "lazy sync" check (`src/vm/vm_var_assign_local_get.rs`
# -- meant to adopt a container established elsewhere in the SAME call frame
# but not yet reflected in the local slot) looked the name up via a full
# parent-chain `Env::get`/`get_sym`. The fast call paths
# (`call_compiled_function_positional_light` and friends, `src/vm/
# vm_call_light.rs`) chain a callee's env as a *scoped child* of the live
# caller env for performance, rather than cloning/flattening it. For a
# recursive call, that parent chain literally IS the caller's own live env
# frame: when the callee's own by-name env mirror for the parameter was
# skipped (the common case for a plain scalar param, `needs_env_sync`
# false), the parent-chain lookup fell through and found the CALLER's own
# same-named boxed parameter cell instead, silently aliasing the callee's
# fresh parameter binding to it. The parameter therefore never actually
# decremented across the recursion, so the sub recursed forever at the Raku
# level -- an unbounded native call chain that overflowed the Rust stack.
#
# Fix: added `Env::overlay_get`/`overlay_get_sym` (overlay-only, no parent
# fall-through) and switched the lazy-sync lookup in `exec_get_local_op` to
# use them, so a local slot never adopts a container from an ancestor call
# frame's same-named variable.
#
# Several of these subs are deliberately written WITHOUT an enclosing
# `lives-ok { ... }` block and WITHOUT referencing any outer/captured
# variable: both of those disqualify a call from the fast light-call path
# (`is_positional_light_call_eligible` / block/closure detection) that this
# bug lived in, so wrapping would silently stop exercising the actual bug.

plan 8;

# --- Test 1: minimal repro (recursion depth 1) does not crash. ---
sub rec(Int $n) {
    my @v = ($n,);
    if $n > 0 {
        rec($n - 1);
    }
}
rec(1);
ok 1, 'minimal repro (recursion depth 1) does not crash';

# --- Test 2: deeper recursion does not crash. ---
sub rec-deep(Int $n) {
    my @v = ($n,);
    if $n > 0 {
        rec-deep($n - 1);
    }
}
rec-deep(30);
ok 1, 'deeper recursion (depth 30) does not crash';

# --- Test 3: each recursion level observes its own decremented parameter
# (not the outermost caller's value -- the aliasing bug made every level see
# the same stale ancestor value, forever). Self-contained: accumulates via
# return value, not an outer variable, to stay on the fast call path. ---
sub rec2(Int $n) {
    my @v = ($n,);
    if $n > 0 {
        return $n ~ "," ~ rec2($n - 1);
    }
    return $n ~ "";
}
is rec2(3), '3,2,1,0', 'each recursive call observes its own decremented parameter, not the ancestor value';

# --- Test 4: the trailing-comma array's element itself must reflect the
# CURRENT call's own value too (not just a bare `$n` read). ---
sub rec3(Int $n) {
    my @v = ($n,);
    if $n > 0 {
        return @v[0] ~ "," ~ rec3($n - 1);
    }
    return @v[0] ~ "";
}
is rec3(3), '3,2,1,0', 'the trailing-comma array element reflects the current call\'s own parameter value';

# --- Test 5: sanity -- the non-trailing-comma form (a plain parenthesized
# scalar, not a List) never triggered the bug, and must keep working. ---
sub rec4(Int $n) {
    my @v = ($n);
    if $n > 0 {
        rec4($n - 1);
    }
}
rec4(3);
ok 1, 'recursion with a non-trailing-comma parenthesized param still works';

# --- Test 6: sanity -- a constant (not the routine's own parameter) inside
# the trailing-comma literal never triggered the bug either. ---
sub rec5(Int $n) {
    my @v = (1,);
    if $n > 0 {
        rec5($n - 1);
    }
}
rec5(3);
ok 1, 'recursion with a trailing-comma array of a constant still works';

# --- Test 7: sanity -- a square-bracket array literal of the own param
# never triggered the bug. ---
sub rec6(Int $n) {
    my @v = [$n];
    if $n > 0 {
        rec6($n - 1);
    }
}
rec6(3);
ok 1, 'recursion with a square-bracket array literal of the own param still works';

# --- Test 8: mutating the trailing-comma array element must not corrupt
# the parameter's own value (matches `raku`: `@v[0]++` does not write back
# to `$n` here -- verified against the reference implementation). ---
sub alias-check(Int $n is copy) {
    my @v = ($n,);
    @v[0]++;
    return $n;
}
is alias-check(10), 10, 'mutating the trailing-comma array element does not corrupt the parameter value';

done-testing;
