# ADR-11318: `PROCESS::` is one store shared by every thread

- **Status**: Accepted (2026-10-03; implemented with #11318)
- Date: 2026-10-03
- Relates to: [ADR-0086](0086-builtin-dynamics-are-not-closure-capture-material.md) (the
  per-interpreter base tier), [ADR-0068](0068-cross-thread-container-writes-need-a-synchronized-store.md)
  (cross-thread stores must be synchronized), #8682 (a `PROCESS::` install must outlive its frame)

## Context

In Rakudo, `PROCESS::` is one stash for the whole process. `$PROCESS::OUT = $h` writes it, and so
does `$*OUT = $h` when no `my $*OUT` is in the dynamic scope, because the lookup falls through to the
process container. Every thread sees the new value, including a thread that was already running.
Test::Output depends on this: it swaps `$PROCESS::OUT` on the main thread while a `start react`
started earlier does the printing (Log::Dispatch `t/020-TTY.rakutest`).

In mutsu, the built-in dynamics are seeded into each interpreter's own env base tier (ADR-0086). A
`$PROCESS::OUT` write went into the writing frame's env overlay. A thread spawned later inherited it
through `clone_for_thread`, but a thread that already existed kept its own copy. The
`process_dynamics` side table (#8682) was per-interpreter too, and each thread clone started with an
empty one.

## Decision

1. **One store per interpreter lineage.** `Interpreter::process_dynamics` becomes a `ProcessStash`
   (`src/runtime/process_stash.rs`). It is an `Arc` around an `RwLock<FxHashMap<key, Entry>>`, and
   `clone_for_thread` gives the child an `Arc` clone. Keys use the env spelling (`*OUT`, `@*x`,
   `%*x`). The lineage is "the process" as far as Raku code can tell: `Interpreter::new` (the
   parse-time probes, a test harness) gets its own store, so unrelated interpreters stay isolated.
   The lock is the synchronized store that ADR-0068 requires. Writes are rare, and reads take an
   uncontended read lock.

2. **A process-level write is published to the stash and mirrored into the writer's env.** This
   covers `$PROCESS::X = v`, `PROCESS::<$X> = v`, a `$*X = v` that lands on the process binding,
   and a `temp $*X` save or restore that does. The env mirror stays because many native readers
   (`$*SPEC`, `$*CWD`, `$*TMPDIR`, ... in `builtins_io*.rs`, `io_env.rs`) read the env directly.
   That keeps them correct on the writing thread, as before. They do not see another thread's
   write; the readers that matter across threads (`say`/`print`/`note`/`get`, and every `$*X`
   expression) go through the stash. A `PROCESS::` write is not mirrored when a `my $*X` is in
   scope, so the lexical binding keeps its value.

3. **Process binding versus lexical binding is decided by identity.** A dynamic read
   (`get_env_with_main_alias_sym`, `get_dynamic_handle`, `GetGlobal` through its slow chain) asks
   the stash first. The stash answers when:
   - the env has no binding for the name, or
   - the env's binding is the same object as one of: the key's value before the first write (the
     base-tier seed, recorded with the entry), its current value, or one of the last 16 values it
     replaced, **and**
   - no `my $*X` marker (`MetaNs::LexicalDynamic`) is visible.

   Values it replaced are counted because a writer's env mirror becomes a stale copy as soon as
   another thread publishes. The window is bounded so that a program swapping handles in a loop
   does not keep every handle alive. Any other binding is lexical and wins: a `my $*X`, a dynamic
   parameter bound to another object, or a redirection a `start` block inherited from its spawning
   scope. The same test decides whether a by-name write publishes.

4. **Nothing published, nothing paid.** A populated flag (`AtomicBool`, set on the first write and
   never cleared) gates every hook. A program that never writes a process-level dynamic pays one
   relaxed load per dynamic read. The `GetGlobal` fast path stays on for it.

5. **`temp $*X` saves the binding, not a deep copy.** `let_saves_push` deep-copies instances so it
   can restore their attributes. For a dynamic scalar, that restored a copy of the handle, detached
   from every other holder. A dynamic scalar now saves the object itself.

## Rejected alternatives

- **Generation sync into each interpreter's base tier.** Each interpreter would copy the stash into
  its own `dyn_base` when it notices a newer generation. The base tier is an immutable `Arc` that
  frames and captured envs share. Replacing it on the running interpreter does not reach saved
  caller envs, and a `start` block's inherited `my $*OUT` is hoisted into the same tier, so a sync
  would overwrite it.
- **Make every process dynamic a shared `ContainerRef` cell from the start.** This is closest to
  Rakudo's model, but every reader of `$*OUT`/`$*ERR`/`$*IN` (and there are many raw `env.get`
  readers) would then have to deref a cell. It would also tie the hot `say` path to closure-boxing
  machinery that ADR-0086 deliberately separated from the dynamics.
- **Decide "lexical" by the `my $*X` marker alone.** Dynamic parameters and other binders do not set
  the marker, so a parameter bound while a process value is published would lose to the stash.

## Consequences

- The snippet in #11318 behaves as in Rakudo. Log::Dispatch `t/020-TTY.rakutest` passes once #11268's
  synchronous delivery (PR #11336) is also in.
- Known gaps. A dynamic parameter or `:=` binding whose value is one of the process binding's
  recognized objects reads as the process binding. An env mirror more than 16 cross-thread
  publishes old reads as a lexical binding. Both are the same object until another thread publishes, so
  only a cross-thread write made during that call can tell them apart. Native readers of a
  dynamic that consult the env directly do not see another thread's write.
- `$*X := v` (bind) still rebinds only the frame's env, not the process container.
