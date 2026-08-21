use Test;

# todo/tickets/thread-param-mask-leaks-on-panic-unwind.md: a named sub's
# scalar parameter is masked out of the cross-thread bare-name shared-var
# lane (`thread_redeclared_vars`/`thread_param_shadow_vars`, see
# `mask_thread_redeclared_params` in src/runtime/runtime_shared_vars.rs) for
# the duration of its call -- so that two unrelated routines merely sharing a
# parameter name don't alias each other across a `start{}`/`await` boundary
# (see t/thread-callee-param-does-not-clobber-caller.t) -- then unmasked once
# the call returns. The unmask used to be a plain statement near the end of
# `call_compiled_function_named_inner` / the method-dispatch equivalent,
# which a Rust panic unwinding through the call body (e.g. an index-OOB /
# integer-overflow panic, same construct t/vm-panic-boundary.t uses) skips
# entirely -- only `Drop` runs on unwind -- leaking the mask into every later
# use of that bare name for the rest of the panicking thread's lineage.
#
# Fixed via `ThreadParamMaskGuard` (src/vm/vm_call_state_guard.rs), whose
# `Drop` undoes exactly the masking on both normal return AND a panic unwind,
# following the same RAII recipe already used for `current_package`,
# `state_scope_id`, `when_matched`, and the pragma/mark-context state (see
# news/2026-08/panic-unwind-call-dispatch-state-raii-guards.md).
#
# The precise internal-state assertion (the two `HashSet`s are empty again
# after a panic-unwound guard, and an ancestor frame's own mask survives)
# is pinned deterministically by the Rust unit tests in
# src/runtime/runtime_shared_vars_tests.rs
# (`thread_param_mask_guard_restores_on_panic_unwind` et al.), which
# construct the guard directly and inspect `Interpreter` state around a
# `std::panic::catch_unwind` -- a corrupted-value repro proved elusive to
# construct purely at the Raku level (the masking only gates a handful of
# narrow internal paths -- see the investigation note in
# news/2026-08/thread-param-mask-panic-unwind-raii-guard.md). This file
# exercises the equivalent end-to-end scenario instead: a
# `start{}`/`await`/named-sub-parameter/`try`/panic combination, following
# the shape of t/vm-panic-boundary.t and t/start-panic-boundary.t.

plan 3;

# 1: a panicking callee whose own parameter shares a bare name with its
# caller's parameter must not corrupt the caller's value, even when the
# panicking call runs inside a spawned `start{}` thread.
{
    sub inner($desc) { my @a; @a[2**64 - 1] = 1; $desc }
    sub outer($desc) { try { await start { inner("inner-value") } }; $desc }
    is outer("outer-value"), 'outer-value',
        'a panicking callee sharing a parameter name does not clobber the caller';
}

# 2-3: the process survives a panic raised inside a param-masked named-sub
# call while the cross-thread shared-var lane is armed, and a later,
# unrelated call sharing that same parameter's bare name still works
# correctly afterward -- i.e. the mask does not get stuck in a bad state for
# the rest of the program.
{
    sub victim($tag) {
        my @a; @a[2**64 - 1] = 1;
    }
    sub harmless($tag) { $tag }

    my $survived = False;
    await start { 1 };            # arm the cross-thread shared-var lane
    try { victim(999) };
    $survived = True;
    ok $survived, 'the process survives a panic inside a param-masked named-sub call';
    is harmless('after'), 'after',
        'an unrelated later call sharing the panicking parameter name still works';
}
