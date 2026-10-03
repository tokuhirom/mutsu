# Cross-thread sharing state leaves `Interpreter`

The fifth subsystem extraction under ADR-10779 (#10779) moved the 13 fields
that decide how variables travel between a thread and the threads it spawns
into a new type, `ThreadSharing` (`src/runtime/thread_sharing.rs`):

- the `shared_vars` store and its two dirty sets;
- the redeclared-name and parameter-shadow masks;
- the transient lane containers;
- the `Lock::Async` and critical-section bookkeeping.

The other subsystems moved so far start every field fresh in a spawned thread.
This is the first one whose thread policy differs from field to field, and that
policy used to be spread over the 438-field literal in `clone_for_thread`. It
now lives in one method, `ThreadSharing::fork_for_thread(captured_scalars)`,
together with the comments that justify each choice:

- the store becomes a child lineage of the parent's (ADR-0010);
- the redeclared set is seeded from the block's captured scalars and the
  parent's parameter shadows;
- the dirty sets are shared with the parent;
- the rest starts fresh.

`ThreadSharing::root()` builds the main interpreter's state. `Interpreter`
went from 353 to 341 direct fields. Only fields moved; the behaviour is
unchanged.
