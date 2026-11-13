# ADR-17337: A pool task starts with warm caches, not a reused interpreter

- **Status**: Proposed
- **Date**: 2026-11-12
- **Issue**: [#17337](https://github.com/tokuhirom/mutsu/issues/17337) (ParaSeq's degree-3
  block `map` costs ~1 ms per 16-element batch)
- **Related**: ADR-15967 §4.1 (the target: long-lived per-worker execution contexts over a
  shared world; this ADR is an interim step towards it), ADR-0020 (shared worker pool;
  "`clone_for_thread` is a per-task contract"),
  ADR-0113 (frame-lexical routines), ADR-0010 / ADR-16595 (what a spawned interpreter shares
  with its parent), ADR-12814 (concurrency memory safety follows rakudo), `docs/security.md`

## 1. Context

Every task the scheduler runs (`$*SCHEDULER.cue`, `start`, `Promise.start`) gets an
`Interpreter` forked from its spawner by `spawn_clone` (`src/runtime/runtime_thread.rs`). The
fork shares the spawner's declarations through copy-on-write snapshots, but every
per-interpreter cache starts empty (`ResolutionCaches::fork_for_thread` returns `Default`), and
the registry write generation starts in a fresh epoch (`fresh_registry_write_gen`), so even the
memos that live in shared compiled chunks miss once per fork.

ParaSeq (`hyperize`/`race`) cues one task per batch and tunes the batch size so that a batch
takes about 1 ms (`$target-nsecs`). When the fixed per-batch cost is near 1 ms, the batch size
stays at its floor and the run degenerates into thousands of tiny tasks: `@a.&hyperize(16,
3).map({ $_ * 2 })` over 200,000 elements made 12,012 batches on mutsu (936 us each) against 72
on rakudo, 6.5 s against 0.2 s. Measured on that workload:

- In-task time of one batch: `BlockMapper.run` ~770 us, `!batch-done` ~260 us. The same
  statements run on the main thread in a loop cost a fraction of that.
- A `start { work() }` whose body is warm on the main thread costs ~450 us more on its first
  call inside the task than on its second; the instructions spent on the first call are
  re-derivations: constructor plans, MROs, user-method probes, method resolution, compiled call
  targets, frame-lexical routine derivations.
- An experiment that kept one interpreter per worker thread and swapped only the task's `env`
  and lineage store into it took the 200k run from 6.5 s to 1.2 s. Swapping subsystems back to
  the fresh fork one at a time showed which state carried that gain: the resolution caches and
  the two frame-lexical tables (`LexicalState::frame_lexical_routines`, `…_closure_bodies`);
  taking every other subsystem (I/O, control, dynamic topic state, VM registers, module state,
  type metadata, dispatch state) from the fresh fork cost nothing measurable.

So the per-task cost is not the fork itself (~0.1 ms on the spawning thread); it is that each
fork starts cold and re-derives what its predecessor on the same worker already knew.

## 2. Decision

### D1. Frame-lexical routine derivations are process-wide

A frame-lexical routine (ADR-0113) is derived from its declaration once per interpreter: the
whole registration path, a registry snapshot and rollback (which deep-copies the copy-on-write
registry a fork shares with its parent), and a copy of the per-interpreter table. Its key,
`FrameLexicalRef` (name, package of the compiled body, body fingerprint), is made of
compile-time facts, so one interpreter's derivation is every interpreter's. A process-wide table
(`vm_frame_lexical.rs`) now serves a declaration that the per-interpreter table misses; the
per-interpreter table stays the first level, so a declaration already seen by this interpreter
never takes the lock. Only derivations whose package came from the definition are published (the
fallback reads the deriving interpreter's current package). An adopted derivation is
indistinguishable from one the parent made before the fork, which is what a fork already
inherits today.

### D2. A worker hands its last task's caches to its next task, gated on registry identity

The fork stays per task (ADR-0020's contract): all program state of a task is the fresh fork's.
What moves between tasks is the derived state only, held per worker thread
(`src/runtime/worker_warm_caches.rs`), never on `Interpreter`:

- `ResolutionCaches`, and the three caches in `TypeState` (`attr_type_constraint_cache`,
  `class_does_role_memo`, `subset_predicate_cache`);
- the registry write generation reached by the task that filled them.

The gate is the registry `Arc` itself:

- **leave**: after a task, its caches are kept only if the interpreter still reads the very
  registry `Arc` it was forked with. The worker holds a second reference to that `Arc` for the
  duration of the task, so any write `Arc::make_mut`s a copy and fails the check; a task that
  wrote its registry leaves nothing.
- **adopt**: before a task, the fresh fork takes the kept caches only if it was forked with that
  same `Arc`, i.e. the spawner has not written its registry in between.

Why this is sound: these caches are memos over the registry. Within one interpreter they are
already required to stay correct across arbitrary execution -- env, frames, current package and
call depth change all the time, so no cache may depend on them without keying on them -- and
the caches that hold an object's address keep that object alive. What a cache cannot survive is
a different registry, and under the gate both interpreters read the same one. Import-scope
changes, `.wrap`, augments and every other event that invalidates resolution memos announce
themselves as registry writes (they renew the function-table version or bump a registry
generation), so they fail the gate too. The generation travels with the caches: it names this
registry (no write happened since it was counted), and the interpreter that counted it is gone,
so no other interpreter counts on from it and the property `registry_gen.rs` relies on -- a
generation value is meaningful for one registry content only -- still holds.

### D3. Relation to ADR-15967 §4.1: an interim step, not the long-lived context

ADR-15967 §4.1 (Accepted) sets the target of the `start` path: one shared world, and a pool
worker that owns a long-lived execution context -- VM registers, frame stack, its per-thread
caches -- created with the worker, not per task. D2 is the part of that target that can land
before the world/execution cut: the caches already outlive the task on their worker. It does
not take the shortcut the experiment in §1 measured -- keeping the previous task's whole
`Interpreter` and swapping the new task's `env` and lineage store into it -- because while the
world is still forked per task, every one of the ~300 fields of the forked subsystems would be a
soundness decision (task's or worker's?), and a field added later would default to the wrong
side silently. D2 inverts that default: everything is the fresh fork's unless it is a listed
cache, and a cache moves only under a check that does not depend on what the cache contains.
When the world is shared (ADR-15967 §4.1's design ADR, after ADR-16595 slice 4), the fork, and
with it this handoff, has no job left; the caches then simply stay in the worker's context.

## 3. Consequences

- The 200k-element degree-3 block map: 6.5 s -> ~1.7 s (rakudo 0.2 s). ParaSeq `t/01-basic`
  29 s -> 9 s, `t/47-deepmap` 59 s -> 14 s, `t/09-map` > 120 s -> 35 s.
- A program whose tasks write the registry (declare classes, `EVAL`, load modules) behaves and
  costs exactly as before.
- The handoff keeps one set of caches, and the registry snapshot they describe, alive per idle
  worker until its next task or its idle exit. That is bounded by the pool size.
- `state` variables are unaffected: they were already shared across threads through the
  process-wide compiled-body cache (matching rakudo).
- No `unsafe`, no new `Send`/`Sync` impls, nothing reachable by another thread: the handoff
  lives in a `thread_local!` and moves values the interpreter already owned.

## 4. Open questions

- The fork still begins a fresh registry-generation epoch when there is nothing to adopt (a
  worker's first task, or after a registry write), so chunk-resident memos keyed on the
  generation (`ClassOperandSite`, the bareword type-object memo) miss once per such task. A fork
  could instead start at its parent's generation, moving to a fresh epoch on its first write.
- The fork itself (`spawn_clone`, ~0.1 ms on the spawning thread for ParaSeq's batcher) remains
  per task.
