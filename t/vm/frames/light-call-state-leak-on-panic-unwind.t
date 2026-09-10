use Test;

# todo/deep/light-call-locals-env-leak-on-panic-unwind.md: the positional-light
# fast call path (`call_compiled_function_positional_light`,
# src/vm/vm_call_light.rs) and its named-arg-capable cousin
# (`call_compiled_function_light`/`call_compiled_function_light_spec`,
# src/vm/vm_call_light_typed.rs) both bypass `push_call_frame`/`run()` for
# performance and manage a large amount of caller-side state (`self.locals`,
# `self.env`, the loop/block-scope save sets, `when_matched`, pragmas, the
# current package/source line, and the routine-stack push/pop) via plain,
# sequential save-before/restore-after statements rather than RAII. None of
# that state is registered on any rollback list
# `recover_call_frames_after_panic` (the top-level `catch_unwind` boundary's
# rollback) knows about, so a Rust panic raised inside the callee body (e.g.
# an index-OOB add-overflow panic, same construct t/vm-panic-boundary.t uses)
# used to unwind straight past every restore statement, leaving the caller
# running on the panicking callee's OWN locals/env. Merely *reading* a
# completely unrelated, same-named outer lexical afterward returned the
# panicking callee's own argument value instead of the outer variable's real,
# untouched value -- silent value corruption, not an exception.
#
# This is the sibling bug to t/readonly-param-mark-panic-unwind.t (fixed via
# `ReadonlyFrameGuard`) and t/thread-param-mask-panic-unwind.t (fixed via
# `ThreadParamMaskGuard`), but for a different, much larger family of
# state that has no single `Box<Cell<_>>` it can be moved behind for an RAII
# guard (`self.locals` and friends are plain fields mutated by thousands of
# call sites throughout the VM). Fixed instead by wrapping just the body-loop
# execution in a local `catch_unwind` inside each function and unconditionally
# restoring all of that state -- exactly as the normal completion path
# already did -- before resuming the unwind. See
# news/2026-08/light-call-state-leak-on-panic-unwind.md.

plan 2;

# 1: the positional-only fast path (`call_compiled_function_positional_light`).
{
    sub victim($desc) {
        my @a; @a[2**64 - 1] = 1;   # deliberate Rust panic: index-OOB add overflow
    }

    my $desc = 'outer-initial';
    try { victim(999) };
    is $desc, 'outer-initial',
        'a panicking positional-light callee does not leak its own $desc into an unrelated outer lexical';
}

# 2: the named-arg-capable light path (`call_compiled_function_light_spec`) --
# requires at least one named parameter to take this code path (see
# `is_light_call_eligible` in src/vm/vm_call_eligibility.rs).
{
    sub victim(:$desc) {
        my @a; @a[2**64 - 1] = 1;   # deliberate Rust panic: index-OOB add overflow
    }

    my $desc = 'outer-initial';
    try { victim(desc => 999) };
    is $desc, 'outer-initial',
        'a panicking named-light callee does not leak its own $desc into an unrelated outer lexical';
}
