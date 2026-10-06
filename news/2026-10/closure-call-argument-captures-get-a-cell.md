# A scalar a closure hands to a call is a shared cell, so an `is rw` write survives a thread

```raku
sub bump-rw($p is rw) { $p⚛++ }
my atomicint $shared = 0;
await (^3).map: { start { bump-rw($shared) } };
say $shared;       # raku: 3    mutsu before: 0
```

A write through an `is rw` parameter is invisible to the compiler's mutation
analysis: the call site only reads `$shared` by name. The creating frame already
knew this about its *own* call arguments (`own_call_arg_sources` keeps such a
variable out of the vouch, so a closure capturing it gets a shared cell), but a
call argument inside a *nested* closure was never reported upward. A variable
named only inside `start` blocks therefore stayed a by-value snapshot; each
worker's `is rw` binding boxed a private cell over its copy, and the creator
never saw an update. Anything that happened to pass the same variable to a call
in the creating frame (`is $shared, 3`, `say id($shared)`) promoted it and hid
the bug, which is why the loss looked shape-dependent. It was not specific to
atomics: `await start { set-rw($v) }` lost a plain assignment the same way.

`CompiledCode::free_var_call_arg_syms` now lists the captured plain scalars that
reach a call as an argument in a closure, at any depth, and bubbles up to the
frame that declares them. When the closure escapes, that frame treats the
capture as captured-and-mutated, so the existing box-on-capture path gives it a
shared cell before the closure runs. A closure that does not escape keeps the
cheap by-value capture, and a callee that only reads its argument is unaffected.

Pinned by `t/concurrency/thread-lock/thread-rw-param-captured-scalar.t` (12 of
its 17 assertions failed before). Found while working #12007; closes #12042. A
separate loss through a named sub that forwards the variable, which needs an
earlier same-named atomic block, is #12075. The design note is ADR-0055 §7.10.
