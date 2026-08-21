use Test;

# todo/tickets/readonly-param-mark-leaks-on-panic-unwind.md: a routine
# parameter gets Raku's default readonly mark on entry
# (`Interpreter::readonly_vars`, via `mark_readonly`/`mark_readonly_sym`) and
# is supposed to lose it again once the call returns, via
# `exit_readonly_frame`. On the positional-light fast call path
# (`call_compiled_function_positional_light` in src/vm/vm_call_light.rs, and
# its typed cousin in vm_call_light_typed.rs) that restore used to be a
# plain, sequential `self.exit_readonly_frame(saved_readonly)` statement near
# the function's end -- this fast path bypasses `push_call_frame`, so the
# scope was invisible to `recover_call_frames_after_panic` (the
# `catch_unwind` boundary's rollback). A Rust panic raised inside the
# callee's body (e.g. an index-OOB overflow, same construct
# t/vm-panic-boundary.t uses) unwound straight past that statement, only
# `Drop` runs on unwind -- so the parameter's bare name stayed marked
# readonly forever, corrupting any later, completely unrelated same-named
# lexical.
#
# This is a SEPARATE mechanism from t/thread-param-mask-panic-unwind.t's bug
# (a different backing registry, `readonly_vars`/`readonly_undo`/
# `readonly_frames` rather than `thread_redeclared_vars`/
# `thread_param_shadow_vars`), and reproduces with zero threading involved.
#
# Fixed via `ReadonlyFrameGuard` (src/vm/vm_call_state_guard.rs), whose
# `Drop` closes the readonly scope on both normal return AND a panic unwind,
# following the same RAII recipe as `ThreadParamMaskGuard` and the other
# guards in that file (see
# news/2026-08/readonly-param-mark-panic-unwind-raii-guard.md).

plan 1;

# The ticket's exact repro -- a panicking callee's own parameter must not
# leave a completely unrelated, same-named OUTER lexical stuck readonly.
#
# This only asserts that the write no longer THROWS (the readonly-mark
# symptom this ticket is about) -- not that `$desc` ends up holding
# 'outer-updated'. The positional-light fast call path
# (`call_compiled_function_positional_light`) also fails to restore
# `self.locals`/`self.env` on this same panic unwind (a separate,
# pre-existing bug unrelated to readonly marking: `$desc` reads back as the
# panicking callee's own argument value, `999`, even on a version of mutsu
# with this ticket's fix applied) -- see
# todo/deep/light-call-locals-env-leak-on-panic-unwind.md.
{
    sub victim($desc) {
        my @a;
        @a[2**64 - 1] = 1; # deliberate Rust panic: index-OOB add overflow
    }

    my $desc = 'outer-initial';
    try { victim(999) };
    lives-ok { $desc = 'outer-updated' },
        'an unrelated outer lexical sharing a panicking parameter name is still writable';
}
