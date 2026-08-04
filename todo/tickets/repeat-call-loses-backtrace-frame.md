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
