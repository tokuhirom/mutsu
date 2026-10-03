# `PROCESS::` is one store for every thread

A write to `$PROCESS::OUT` (or `PROCESS::<$x>`, or a `$*OUT = ...` that finds no `my $*OUT` in scope)
is now seen by every thread, including one that was already running. This is how Test::Output's
`output-like` captures lines that a `start react` prints. Before, mutsu kept process-level dynamics
in each interpreter's own env, and a thread clone copied them only at spawn time (#11318).

The process stash (`src/runtime/process_stash.rs`) is shared through `clone_for_thread`. A
process-level write goes only to the stash. A read reaches the stash when the env's binding is still
the original seeded value (an identity check), so `my $*OUT`, dynamic parameters and a `start`
block's inherited redirection still win. The design is
[ADR-11318](../../docs/adr/11318-process-stash-is-one-store-per-process.md).

Single-threaded fixes that came with it:

- `PROCESS::<$OUT> = $h` now redirects `say`.
- A `$PROCESS::OUT` swap made inside a sub outlives that sub's frame.
- `temp $*OUT = $h` restores the original handle object instead of a detached copy.
