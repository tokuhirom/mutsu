# An unclaimed `Proc::Async` stream is inherited now, not swallowed

A `Proc::Async` child's output vanished whenever the program never claimed the
stream:

```raku
my $prog = Proc::Async.new(:w, 'hexdump', '-C');
my $promise = $prog.start;
await $prog.write(Buf.new(12, 42));
$prog.close-stdin;
await $promise;
```

Rakudo prints the hexdump; mutsu printed nothing. Same for a `bind-stdin` chain
(`$cat.bind-stdin: $echo.stdout`, nothing tapped) and for `bind-stdin` from a
file handle.

## Root cause

`.start()` (`src/runtime/native_proc_async.rs`) unconditionally configured
`cmd.stdout(Stdio::piped()).stderr(Stdio::piped())`. When nothing ever drained
that pipe, the reader thread read the bytes into a channel and a `collected_*`
string that no one consumed, and the output was simply dropped on the floor.

## The rule Rakudo actually follows

Established by probing rakudo 2026.06 rather than guessing, because "tap
presence" turns out to be the wrong predicate:

- Neither stream claimed → the child inherits the parent's real stdout/stderr,
  per stream and independently (a claimed stdout with an unclaimed stderr sends
  the first to the tap and the second to the parent's stderr).
- The decision is made by the **accessor**, not by the tap. A Supply fetched with
  `$p.stdout` *before* `.start` and tapped *after* it still receives the output;
  conversely, calling `$p.stdout` and never tapping it makes Rakudo hang waiting
  for a tap, which is proof the accessor alone already switched the stream to a
  pipe.

## The fix

`.start()` now pipes a stream only when it was claimed — `.stdout`/`.stderr`
(which record `stdout_selected`/`stderr_selected`), the merged `.Supply`, or
`bind-stdout`/`bind-stderr` — and uses `Stdio::inherit()` otherwise. Taps
registered directly on any of the three supply ids are folded into the same
union as a safety net, because not every claim writes a `*_selected` flag: the
read-only `native_proc_async` accessor path has no `&mut self`. A stream that is
not piped also gets no supply channel, so no receiver is left parked in the
global map with nothing able to feed it.

One companion fix was required. `whenever $proc { ... }` means
`whenever $proc.Supply` (the merged stdout+stderr stream), but
`run_whenever_with_value` only coerced `Supplier`/`Supplier::Preserving` to a
Supply and passed a `Proc::Async` instance straight through — so it registered
no tap on any of the proc's supplies and claimed nothing. Under the old
always-pipe behaviour that merely lost the output silently; under inheritance it
would have leaked the child's output to the terminal (caught by
`roast/S17-procasync/stress.t`). `Proc::Async` now joins the same coercion, which
claims the streams exactly as Rakudo does.

## Known remaining gap

`whenever $proc { ... }` / `whenever $proc.Supply { ... }` *inside a `react`
block* still never runs its body: the merged Supply has no channel feeding it, so
react's drive loop has nothing to poll, and the await-time `replay_proc_taps`
path that serves the non-react `$proc.Supply.tap(...)` case is not reached from
inside react. This is pre-existing (it reproduces identically without any of the
changes here) and is filed separately as
`todo/tickets/procasync-merged-supply-not-delivered-in-react.md`.

## Test

`t/proc-async-divergences.t` asserts the rule rather than one repro. The
passthrough cases run a child under `$*EXECUTABLE` that itself spawns an
unclaimed `Proc::Async`, and capture *both* of the middle process's streams — so
"did it reach the parent's real handle" becomes an ordinary string assertion with
no reliance on terminal state, sleeps, or ports. Interleaving between the two
inherited streams is deliberately not asserted (it is genuinely unordered); what
is asserted is that each marker lands on its own stream and that a claimed stream
is *not* also echoed to the parent handle.
