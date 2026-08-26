# `Promise.cause`'s backtrace no longer repeats a frame

```raku
my $promise = Promise.start({ die "Broken Promise" });
try $promise.result;
say $promise.cause;
```

mutsu printed the same frame twice:

```
Broken Promise
  in block <unit> at f.raku line 1
  in block <unit> at f.raku line 1
```

`.backtrace.list` agreed — two `Backtrace::Frame`s, identical `subname`, `file`
and `line`.

## Root cause: a phantom mainline frame on a worker thread

Not a Promise-specific formatting bug, and not inside `Backtrace` rendering
either. Both backtrace builders in `vm/vm_helpers.rs` finish by appending a
synthetic `<unit>` frame beneath the outermost real frame, suppressed only when
the bottom frame already *is* the mainline boundary:

```rust
f.name == "<unit>" || (f.name.is_empty() && !f.is_block)
```

A `Promise.start` block's callframe is named `<pointy-block>` (that is the
marker `vm_closure_dispatch.rs` pushes for every bare and pointy block, so
`&?ROUTINE` skips it), which matches neither disjunct — so a `<unit>` frame was
appended. It rendered at the block's own call-site line, which for
`Promise.start({ ... })` is the same line as the block, hence a visually exact
duplicate.

The deeper point is that the append is *categorically* wrong here:
`clone_for_thread` starts a worker with `routine_stack: Vec::new()`, so the
bottom frame is the thread's entry block and there is **no** mainline `<unit>`
beneath it to describe. The suppression condition simply had no way to know
that.

## The fix

The predicate is now one named helper, `stack_bottom_is_mainline_unit`, shared
by both builders, with the thread case as an explicit second reason to suppress:
a thread-clone interpreter (`is_thread_clone()`, the flag that already
distinguishes worker interpreters elsewhere) has no mainline unit under its
stack bottom, so nothing is appended.

This is a fix to how frames are *captured*, not to how `Backtrace` renders them,
so it does not touch the `Backtrace.full`/`.nice`/`next-interesting-index` work
happening in parallel. The mainline behaviour is unchanged: a bare block called
from the main program still gets its enclosing `<unit>` frame, verified against
the pre-existing rendering.

## What is deliberately *not* matched

Rakudo shows the anonymous `Promise.start` block as an unnamed `in block ` frame
where mutsu still shows `in block <unit>`, and rakudo's structured `.list` for
this case has ten frames (including `throw`, `die`, `run-one` and
`THREAD-ENTRY` setting frames) against mutsu's one. mutsu has no Raku CORE
setting and a coarser callframe model, so `t/promise-keep-break-semantics.t`
asserts the *absence of duplication* structurally — no backtrace line repeats
verbatim, and at least one frame is present — rather than pinning rakudo's exact
frame list.
