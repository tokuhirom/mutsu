use Test;

# t/panic-recovery-call-frames.t pins the fix that recovers `call_frames` /
# `locals` / `upvalues` / `env` / the value stack after a Rust panic is caught
# at a `try`/`EVAL` boundary. But several further pieces of call-dispatch
# state are saved/restored OUTSIDE that mechanism, as a plain Rust local in
# the dispatch function itself (`call_compiled_closure_with_topic`,
# `call_compiled_function_named_inner`): most visibly `current_package`,
# switched to the callee's declaring package for the call's duration and
# restored with a manual `self.set_current_package(saved)` statement near the
# function's end. A Rust unwind skips straight past such a statement (only
# `Drop` runs on unwind), so `current_package` was left as whatever package
# the panicking callee happened to run in, and the very next unqualified call
# resolved against the WRONG package ("Unknown function: ...").
#
# See todo/deep/panic-unwind-leaks-side-channel-call-state.md (now resolved
# via RAII guards -- CurrentPackageGuard et al. -- whose `Drop` runs on both
# normal return and a Rust panic unwind).

plan 2;

{
    # Exercises `call_compiled_function_named_inner`'s package guard
    # directly: `P::boom()` is called BY NAME from GLOBAL, switching
    # `current_package` to `P` for its duration; it panics; `try` catches it
    # at the `run_range_guarded` boundary. The very next statement calls an
    # unqualified sibling declared in GLOBAL -- if `current_package` was left
    # as `P`, this fails to resolve.
    package P {
        our sub boom() {
            my @a;
            @a[2**64 - 1] = 1;
        }
    }
    sub sibling-in-global() { 99 }
    try { P::boom() };
    is sibling-in-global(), 99,
        'current_package is restored after a panic caught mid-call to a package-qualified sub';
}

{
    # Exercises `call_compiled_closure_with_topic`'s package guard: a
    # closure created in GLOBAL (`$code`) is invoked (via `$code()`) from
    # inside a named sub `run-it` declared in package `Helper`. The closure
    # panics; `try` catches it; `run-it` then calls a sibling declared in ITS
    # OWN package (`Helper::sibling`) by unqualified name. If the closure
    # call's package guard left `current_package` at `GLOBAL` (the closure's
    # own package) instead of restoring `Helper`, this unqualified call fails
    # to resolve.
    package Helper {
        our sub run-it($code) {
            try { $code() };
            sibling();
        }
        our sub sibling() { 42 }
    }
    my $code = -> { my @a; @a[2**64 - 1] = 1 };
    is Helper::run-it($code), 42,
        'current_package is restored after a panic caught mid-call to a closure invoked from another package';
}
