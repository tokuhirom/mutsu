# A repeat call to the same routine now keeps its `in sub` backtrace frame

`sub f() { die "boom" }` called from a loop lost its `in sub f` backtrace
frame — and reported the wrong call-site line for the frame that remained —
on the SECOND and every later call, while the first call reported correctly.
Not specific to `die`: any error escaping a routine (a type-check failure,
method-not-found, `take` without `gather`) lost the frame the same way, which
is why `t/take-without-gather.t` deliberately called a fresh routine for its
assertion rather than reusing one.

Root cause: the first call to a routine goes through
`call_compiled_function_named_inner`, which pushes a `RoutineFrame` onto the
routine stack that `Interpreter::build_backtrace_string` reads to render a
backtrace. A repeat call to the same call site is routed through the
specialized `call_compiled_function_fast` (`src/vm/vm_call_fast.rs`) instead —
a path that exists specifically to avoid per-call overhead in tight loops —
and it did not push a frame at all, so the information the backtrace needed
simply was not there.

The straightforward "push it only when the body actually errors" fix does not
work: the backtrace string is built at the *raise* site, deep inside the
body, from the live routine stack at that moment. By the time an `Err`
propagates back up to `call_compiled_function_fast`, the string has already
been formed without the frame. The frame has to exist *before* the body runs.

A straight copy of the slow path's frame push was too expensive to do
unconditionally: `RoutineFrame` held several `String`/`Option<String>` fields
(`package`, `lexical_package`, `name`, `file`, `def_file`), so pushing one
cost multiple heap allocations per call — exactly the cost the fast path
exists to avoid.

The fix interns those fields as `Symbol` (`Copy`, and the strings involved —
constant-pool names, `SubData::package`/`name`, `CompiledFunction::package`/
`source_file` — are almost always already interned somewhere), so a push is
now a plain `Vec::push` with no allocation on a warm call: `Symbol::intern`
memoizes per string in a thread-local cache, and the call site already has a
`Symbol` for the routine name (`code.const_sym`) for free. `RoutineFrame` is
now `Copy` itself. With the push now cheap, `call_compiled_function_fast`
pushes a frame unconditionally like every other call path, closing the gap.
This touched roughly 80 `routine_stack` call sites across the interpreter —
mostly readers (`build_backtrace_string`, `build_backtrace_value`, the
`caller()` builtin, `&?ROUTINE`, `anon_state_key`, package-scope resolution)
that previously read `String` fields directly and now resolve a `Symbol` back to
`&str`/`String` at render time via `Symbol::as_str()`/`Symbol::resolve()`.

Pinned by `t/repeat-call-backtrace-frame.t`, which calls the same failing
routine 3 times in a loop and asserts every call's backtrace has the `f`
routine frame with the correct call-site line — a single call would not have
caught the regression, since only a repeat call takes the fast path that lost
the frame.

This is a hot-path change (the per-call push on `call_compiled_function_fast`,
which every 2nd+ call to a simple 0-arg routine takes). The qualitative perf
check: `roast/S04-declarations/state.t` test 42 (a 2,000,000-iteration loop)
still completes comfortably within its CI time budget locally after the
change. The authoritative numeric verdict is this PR's own bench-CI run on
`main` post-merge (`bench-history.tsv` on the `bench-data` branch) — per this
repo's convention, no specific before/after millisecond number is asserted
here from a local run, since local wall-clock is unreliable whenever a
sibling checkout on this machine is building.

While auditing every `RoutineFrame` construction site, the same missing-frame
bug was found in the method-dispatch fast path (`call_compiled_method_fast`
in `src/vm/vm_method_dispatch.rs`), which likewise never pushes a
`RoutineFrame`. That is out of scope for this fix (the ticket was about the
sub fast path) and is recorded separately for follow-up.
