# ADR-17697: A type minted at run time on a worker thread is reachable from its sibling threads

- **Status**: Proposed (2026-11-13; awaiting the maintainer's decision)
- **Date**: 2026-11-13
- **Issue**: [#17697](https://github.com/tokuhirom/mutsu/issues/17697) (Red t/104 subtest 6)
- **Related**: ADR-11456 (run-time mixin types are published process-wide as recipes),
  ADR-12814 (memory safety under concurrency follows rakudo), ADR-0019 §2 (one registry per
  interpreter), ADR-0010 (spawn lineage), `docs/security.md`

## 1. Context

Every thread runs on its own interpreter. `TypeState::fork_for_thread` hands the child a
copy-on-write snapshot of the parent's declaration registry, taken when the thread spawns. A type
created after that, on any thread, lands in that thread's registry only.

`Metamodel::*HOW.new_type` represents the type it mints as a by-name `Value::package(name)`, and
method dispatch on it (`.new`, any method) resolves the name through the *current* thread's
registry. So a type minted on worker thread A after sibling B has spawned is unknown to B, even
when the program synchronizes the mint correctly:

```raku
class Cache {
    has $!rs = Any;
    method get {
        return $!rs unless $!rs === Any;
        state Lock $lock .= new;
        $lock.protect({
            if $!rs === Any {
                my $x := Metamodel::ClassHOW.new_type: :name('Shared::ResultSeq');
                $x.^compose;
                $!rs := $x;
            }
            $!rs
        })
    }
}
my $c = Cache.new;
say (await (^16).map(-> $i { start { $c.get.new; 'ok' } })).unique.sort.join(",");
# raku:  ok
# mutsu: Could not find symbol '&ResultSeq' in 'GLOBAL::Shared'   (deterministic)
```

Red's `MetamodelX::Red::Model.rs-class` is exactly this shape: concurrent first access to a
projection model mints its `ResultSeq` class under `protect-model-composition`'s lock, caches it,
and the other workers call `.new` on the cached type (Red t/104, "Concurrent first access
publishes one class per model").

The mint is synchronized, so ADR-12814 §2.1 (1) applies: this program is entitled to a correct,
memory-safe answer. The registry is mutsu-internal state the program cannot synchronize, so §2.1
(2) applies too: whatever mutsu does to share it must be sound unconditionally.

ADR-11456 met the same shape for one kind of run-time type, the mixin `C+{R}` created by `does`
on a shared object. It did not make the registry shared. The creating thread publishes the type's
*recipe* to a process-wide table (`PublishedMixins`, shared by every fork), and a thread that
meets the name with no registry entry composes the type into its own registry from the recipe
(`adopt_published_mixin`, called from the dispatch, type-matching and `.new` miss paths).

## 2. Options

1. **One registry shared by every thread.** Replace the per-thread COW snapshot with a single
   `RwLock`ed registry. This answers every cross-thread visibility gap at once. But it is the
   largest change by far: every registry write becomes visible to all threads mid-flight (a class
   being composed on one thread observed half-built on another), it undoes ADR-0019's "one
   registry per interpreter", and it puts a shared write lock on paths that are lock-free today.
   Every in-flight-declaration invariant would need re-auditing under ADR-12814 §2.1 (2).
2. **Publish the minted type's registry entries at compose; adopt them on a miss (ADR-11456's
   pattern).** When `compose` finishes on a `new_type`-minted type, the minting thread copies the
   registry entries keyed by that type's name into a process-wide `PublishedMintedTypes` table:
   its `ClassDef`, `class_how_values`, `declared_native_how`, composition and parent records, and
   the package-path entries a qualified name needs. On a registry miss for a name, the
   dispatch/type-resolution miss paths that already consult `PublishedMixins` also consult this
   table and copy the entries into the current thread's COW registry. Publication happens only
   after compose completes, so a sibling never adopts a half-built type.
3. **Publish a replayable recipe instead of the entries.** Record the metamodel call log
   (`add_parent`, `add_role`, `add_method`, `add_attribute`, `compose`) and replay it on adopt, as
   ADR-11456 replays `base+{roles}`. This is clean for the native `ClassHOW` calls, but a minted
   type's methods are closures added at run time, and a user HOW subclass runs user code in those
   calls. Replaying that code on another thread could repeat its side effects, so this is
   unsound in general.
4. **Hold minted types as direct object references, not by-name packages.** Dispatch would read
   the type's definition off the value itself, bypassing the registry. This is closest to
   rakudo, but it changes the representation of every `new_type` result and every registry
   consumer that resolves by name (`.^name` lookups, smartmatch, stash, `::()`). It is a
   campaign, not a fix.

## 3. Proposed decision

**Option 2.** It is the smallest change that makes a synchronized program correct, and it reuses
a mechanism already accepted and in production (ADR-11456):

- The table is mutsu-internal sharing. It is guarded by an `RwLock`, like `PublishedMixins`, holds
  only complete post-compose snapshots, and is written once per name (first publication wins).
  This satisfies ADR-12814 §2.1 (2) without `unsafe` and without a new `Send`/`Sync` impl.
- The entries hold `Value`s (method closures, HOW instances). Values already cross threads through
  the shared-variable lanes (ADR-0010/0068), and the cycle collector is sound across parallel
  interpreters (`tests/gc_parallel_interpreters.rs`). So sharing them adds no new kind of
  cross-thread reference. The type object itself already crosses today; only its definition is
  missing.
- An adopting thread *copies* the entries into its own COW registry. Writes on either side after
  adoption stay thread-local, as for every other run-time declaration, so ADR-0019's per-interpreter
  registry is untouched.
- The miss-path probe costs one relaxed atomic load when nothing was ever published (the
  `PublishedMixins::any` pattern), so programs that never mint types on worker threads pay nothing.

Scope: only types minted by `Metamodel::*HOW.new_type` and then composed. Declared types already
exist in every fork's snapshot when they are declared before the spawn. A type *declared* (not
minted) on a worker thread is out of scope here and would need its own issue.

## 4. Consequences

- Red t/104 subtest 6 and the Red-free repro above match rakudo.
- A program that mints types on worker threads *without* synchronization still gets rakudo's
  ADR-12814 answer. It may race, but mutsu's own table stays sound.
- A second process-wide publication table now exists next to `PublishedMixins`. If more run-time
  type kinds need the same treatment, a later ADR should merge the two into one "published runtime
  types" table rather than add a third.

## 5. Security review (`docs/security.md`)

- No new code execution path: adoption copies data and runs no user code. That is the reason
  option 3 was rejected.
- No `unsafe`, no `Send`/`Sync` impl, no `&self → &mut` access without a lock. The table is
  behind `RwLock`.
- The table grows by one entry per distinct minted name. That is bounded by what the program
  mints, the same bound `PublishedMixins` has, and is not driven by untrusted input any more than
  the registry itself is.

## 6. Open questions for the maintainer

- Option 2 (adopt published entries) or option 1 (a shared registry)? Option 2 is proposed as the
  bounded fix; option 1 is the general one.
- Should `PublishedMixins` and the new table be merged now, or later when a third kind appears?
