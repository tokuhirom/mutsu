# A `next`/`last`/`redo` with no enclosing loop is not catchable

In rakudo a loop-control statement that finds no loop to act on becomes an
ordinary, catchable exception at the point it is raised:

```raku
try { my $i; { $i++; next; $i--; } };
say $!.^name;      # X::ControlFlow
say $!.message;    # next without loop construct
say $!.illegal;    # next
say $!.enclosing;  # loop construct
```

mutsu raises `RuntimeError::next_signal()` — a control signal, not an exception —
and `try`/`CATCH` deliberately let control signals through
(`vm_try_catch_ops.rs`, the `is_last() || is_next() || is_redo() || …` arm). The
signal therefore escapes every handler and only surfaces at the top of the
program as `Runtime error: X::ControlFlow`, uncatchable:

```
$ mutsu -e 'say (try { my $i; { $i++; next } }).^name'
Runtime error: X::ControlFlow
```

## Why the obvious fixes are wrong

`try` cannot decide this on its own, and neither can the compiler:

- **A control signal legitimately crosses routine and `EVAL` boundaries.**
  `sub f { next }; for 1..3 { f() }` iterates three times in rakudo *and* in
  mutsu, and so does `for 1..3 { EVAL 'next' }` — measured 2026-08-03. So
  neither the routine boundary nor the `EVAL` boundary may convert the signal.
- **Compile-time "there is no lexically enclosing loop" is not sufficient**, for
  the same reason: the loop that handles the signal is usually in another
  routine entirely.
- **Converting at the top of the interpreter loop** (the way an uncaught
  `CX::Return` becomes `X::ControlFlow::Return` in `vm_run_loop.rs`) fixes the
  top-level *message* but not catchability: by then the `try` has already passed
  the signal on, and unwinding is one-way.

The only correct discriminator is the *dynamic* one rakudo uses: is there a
loop-control handler anywhere up the dynamic chain right now? mutsu has no such
registry — every loop construct handles the signal ad hoc by matching
`Err(e) if e.is_next()` on its body's result.

## What it would take

An `Interpreter::loop_handler_depth` counter, incremented for the dynamic extent
of every construct that handles a loop-control signal and consulted at the raise
site (throw a real `X::ControlFlow` when it is zero, exactly as
`react_done_signal()` already pre-builds its `X::ControlFlow` for the `done`
case). The catch sites to instrument, from
`git grep -l 'is_next()\|is_last()\|is_redo()'`, span 22 files:

```
runtime/builtins_collection_deepmap.rs  runtime/builtins_reduce.rs
runtime/calls.rs                        runtime/methods_collection_ops/grep.rs
runtime/methods_seq_dispatch.rs         runtime/native_supplier_methods.rs
runtime/native_supply_mut_methods.rs    runtime/resolution_map_grep.rs
runtime/resolution_map_grep_rw.rs       runtime/sequence.rs
runtime/supply_promise.rs               vm/vm_control_ops.rs
vm/vm_for_loop_body.rs                  vm/vm_for_loop_intrange.rs
vm/vm_for_loop_lazy.rs                  vm/vm_helpers_lazy_pull.rs
vm/vm_loop_cstyle_repeat.rs             vm/vm_misc_block.rs
vm/vm_react_loop.rs                     vm/vm_react_subscriptions.rs
vm/vm_react_supply_helpers.rs           vm/vm_try_catch_ops.rs
```

The sweep has to be **complete**: a missed site turns a `next` that construct
would have handled into a thrown `X::ControlFlow`, silently breaking that loop.
That is a compatibility regression, not just a missing feature, which is why
this is filed as a deep item rather than done piecemeal. Deterministic, so roast
catches it — but the whole sweep belongs in one PR.

## What it blocks

Three whitelisted roast files fail under `MUTSU_REAL_TEST=1` on this alone
(the real `Test`'s `throws-like` runs its argument inside a `subtest`'s `CATCH`,
which the signal walks straight through, aborting the file):

- `roast/S04-statements/do.t` — `throws-like 'my $i; { $i++; next; $i--; }', X::ControlFlow`
- `roast/S04-statements/redo.t`
- `roast/S04-blocks-and-statements/pointy.t`

mutsu's native `throws-like` special-cases control signals, which is why the
same files pass under the native provider today.
