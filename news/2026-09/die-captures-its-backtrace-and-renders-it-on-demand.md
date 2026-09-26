# `die` captures its backtrace and renders it on demand

Every `die` (and every other runtime error that gets a backtrace attached)
used to render the whole call stack twice at the throw site: once as the
backtrace text on the error, and once as the structured `Backtrace` object on
the exception, with a `Backtrace::Frame` instance per frame. A `try { die }`
inside deep recursion, a common error-handling pattern, paid for the whole
stack on every throw even when nothing ever read `.backtrace` (#9172). Rakudo
builds its backtraces lazily.

Now a throw only captures the stack, and both forms are rendered the first
time something reads them:

- `RoutineStack::snapshot` hands out an immutable, `Arc`-linked snapshot of
  the routine stack. Nodes are built on demand and shared between snapshots
  (frames are never modified in place), so each frame is copied at most once
  in its lifetime and a snapshot is O(1) amortized.
- The error's backtrace text (`RuntimeError::backtrace`) renders from the
  capture on its first read.
- The exception's `Backtrace` object is an instance whose attributes come
  from a `LazyAttrSource` (`src/value/lazy_attrs.rs`). The first access to
  its attribute map fills them in, so every reader sees an ordinary
  `Backtrace`. The GC traces the raw map, which never materializes anything.
- `CATCH` / `try` no longer render the text and parse it back into a
  `Backtrace` just to throw it away when the exception already carries its
  own.

Measured on a release build, 500 `try { die "x" }` at recursion depth 1000
and 2000 (paired runs, second run quoted):

| depth | before | after |
| --- | --- | --- |
| 1000 | 1.44 s | 0.0027 s |
| 2000 | 3.25 s | 0.0028 s |

`scripts/vm-complexity-check.sh die` now throws 20000 times, so that the
O(depth) recursion in its own setup stays a small share of the timed body.
It reads a ratio of 1.12 when the depth doubles.
