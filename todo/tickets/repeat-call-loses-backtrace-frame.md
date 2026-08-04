# The second call to a routine loses its `in sub` backtrace frame

```raku
sub f() { die "boom" }
for 1..3 -> $i {
    try { f() };
    say "call $i: ", $!.backtrace.Str.subst("\n", " | ", :g);
}
```

```
call 1:   in sub f at t.p6 line 1 |   in block <unit> at t.p6 line 3
call 2:   in block <unit> at t.p6 line 1
call 3:   in block <unit> at t.p6 line 1
```

rakudo names `in sub f` on every call. mutsu names it only on the first, and the
remaining frame's line number is wrong as well (line 1, the `sub` declaration,
rather than the call site).

The first call goes through `call_compiled_function_named_inner`, which pushes a
routine frame; later calls are dispatched by `call_compiled_function_fast`
(`src/vm/vm_call_fast.rs`) — the specialised path `exec_call_func_op` selects
once the call site has been seen — and that one does not push one. Since
`Interpreter::build_backtrace_string` reads the routine stack, the frame simply
is not there to report.

Not specific to `die`: it is whatever error escapes the routine, so `take`
without a `gather`, a type-check failure and a method-not-found all lose the
frame the same way. It shows up as soon as a test file calls the same failing
routine twice, which is why `t/take-without-gather.t` deliberately uses a
routine it has not called before for its backtrace assertion.

The fix is to give the fast path the same frame bookkeeping the named path has,
which means paying for the push/pop on the path that exists precisely to avoid
per-call overhead — so it wants a cheap form (push a frame only when the callee
can fail? record the callee name in a side slot the backtrace builder reads?)
rather than a straight copy of the slow path.

## Why "push it only on the error path" does not work (measured 2026-08-04)

The obvious cheap fix — notice `Err(e)` coming back from the body loop in
`call_compiled_function_fast` and add the frame there — cannot work. The
backtrace is built at the **raise** site, deep inside the body, from the live
routine stack; by the time the `Err` reaches this function the string is already
formed. The repro shows both halves of that: call 2 loses `in sub f` *and*
reports `in block <unit>` at line 1 (the `sub` declaration) instead of line 3
(the call site), because the whole trace came from a stack that never had the
frame.

So the frame has to be pushed *before* the body runs, and the cost is real:

```rust
pub(crate) struct RoutineFrame {
    pub package: String,
    pub lexical_package: Option<String>,
    pub name: String,
    pub line: Option<u32>,
    pub file: Option<String>,
    ...
}
```

Three `String`s and two `Option<String>` — several allocations per call, on the
path whose entire purpose is to avoid exactly that. This is why the fast path
skips it, and why a straight copy of the slow path is the wrong answer.

### The shape worth pricing first

Make the frame cheap to push rather than pushing it conditionally: `package` /
`name` / `file` are all interned-able, so `Symbol` (Copy) would make the push a
plain `Vec` push with no allocation, and the fast path could then just do it
unconditionally like every other path. That also lines up with the symbol-intern
direction already open in the method-call perf work.

Size: **13 `RoutineFrame { … }` construction sites**, but ~81 `routine_stack`
references overall, so the reader side (backtrace rendering, `callframe`
introspection, `anon_state_key`) is where the work is.

**Measure it on the bench CI, not locally**: this is a per-call cost on the
hottest call path, and local wall-clock on this machine is unreliable whenever a
sibling checkout is building (load average 15–20 on 12 cores is normal here).
