# ADR-12814: Memory safety under concurrency follows rakudo's model

- **Status**: Accepted (user decision 2026-10-23)
- **Date**: 2026-10-23
- **Issue**: [#12814](https://github.com/tokuhirom/mutsu/issues/12814) (unguarded reads of a
  shared `has @!a` race concurrent mutation into panics and heap corruption)
- **Related**: `docs/security.md` (the Threads row and the memory-safety invariants this
  changes), ADR-0068 (cross-thread container stores), ADR-0010 (spawn-lineage sharing),
  ADR-12529 and ADR-11209 (designs that cite the old invariant; both stay valid).

## 1. Context

`docs/security.md` promised more than rakudo does: "a Raku data race may give a wrong answer, never
a use-after-free or a corrupted `NanBox`". That covered every program, including one that mutates
a single container from two threads with no synchronization at all.

#12814 measured what that promise costs and what rakudo does on the same programs. Six threads
call methods of one object with no lock; the methods `unshift`/`pop`/`append`/`splice`, read,
`join` and reassign the same `has @!arr`:

- **mutsu** crashes: `malloc_consolidate(): unaligned fastbin chunk`, slice-index panics in
  `NanBox::as_list_slice` / `ArrayData::as_slice`, and a read of a freed string.
- **rakudo 2026.09 / MoarVM** also crashes on the same shapes: `MoarVM panic: Internal error:
  zeroed target thread ID in work pass` and segfaults (exit 139), after hundreds of wrong reads.

Raku leaves this to the program. The documentation says reifying or mutating an `Array` from more
than one thread "is not safe" (`Type/Array`), and that `Lock`, and the classes built on it
(`Promise`, `Supply`, `Channel`), are how shared data is protected (`Language/concurrency`).

Keeping the stricter promise would mean copy-on-write plus an atomic publish for every node
another thread can reach, or a lock around every read of a shared root. Either is a rewrite of the
`gc_contents_mut` write routes, with a cost on contended `push`. And it would buy behaviour that no
Raku implementation offers and no correct Raku program needs.

## 2. Decision

**mutsu's memory-safety guarantee under concurrency is rakudo's.** In principle, where the Raku
language leaves a behaviour to the program, mutsu does not promise more than rakudo does.

### 2.1 What stays guaranteed

1. **A program that synchronizes its shared data is memory-safe.** Synchronization here is what
   Raku provides: `Lock` / `Lock::Async`, `Semaphore`, `cas` and the atomic ops, hand-off through a
   `Channel`, `Supply` or `Promise` (`await` orders the work before it), a `react` block
   serializing its `whenever`s, and `hyper`/`race` over data the workers do not share. Every
   memory access mutsu makes on such a program's behalf is sound, including ones the program
   cannot see.
2. **mutsu's own sharing is sound unconditionally.** The program cannot synchronize state it does
   not know exists, so mutsu-internal structures must be safe whatever the program does:
   - registries, caches and memo tables;
   - lazy materialization;
   - reference counts and the cycle collector;
   - the scheduler and thread pool;
   - the spawn-lineage and shared-variable lanes that mutsu itself introduces (ADR-0010, ADR-0068);
   - a closure's captured cells, when the program does not write them concurrently.
3. **Untrusted input never corrupts memory.** The data-parser row of `docs/security.md` is
   unchanged: decoding a value aborts nothing and corrupts nothing, on any number of threads.
4. **The Rust rules stay.** Every `unsafe` block carries a `// SAFETY:` comment. No new
   `unsafe impl Send`/`Sync` is added. A raw pointer never outlives its guard. These keep
   guarantees 1-3 reviewable.

### 2.2 What is not guaranteed

A program that mutates one container from more than one thread without synchronization, or reads
it while another thread mutates it, has no defined result. As on rakudo, its own process may give
wrong answers, panic or crash. mutsu does not add copy-on-write, locks or snapshots to make such a
program memory-safe. Where a wrong answer or a catchable exception costs nothing over a crash, it
is preferred, but no mechanism is built for it.

This is not a trust-boundary change. Running a script was already trusted (`docs/security.md`:
it can `run`, `shell` and `use NativeCall`). A racing script can corrupt only its own process,
which it could already do.

## 3. Consequences

- `docs/security.md` changes in two places. Its Threads row says "as rakudo implements them", and
  its memory-safety invariant on `gc_contents_mut` is restated as guarantees 1 and 2 above. The
  AGENTS.md hard rule that quoted the old sentence is restated the same way.
- **#12814 is closed as not planned.** Its crashes are in unsynchronized programs, which is
  case 2.2. The designs in #12814 (copy-on-write plus atomic publish, a lock on every read, celled
  attributes) are no longer required. They stay available as optimizations or for other reasons.
- **Existing locks stay.** The ADR-11209 attribute-cell locks and the ADR-0068 funnels remain
  correct, and are required wherever they protect mutsu's own sharing (guarantee 2). Whether a
  given guard only ever protected the unsynchronized case, and could go, is a per-site question
  answered with a measurement, not a blanket removal.
- **New finding classification.** A crash found by a thread-stress script is classified by
  rerunning the script with the shared data synchronized (`Lock.protect` around each access):
  - if the crash persists, it is a mutsu bug (guarantee 1 or 2);
  - if it goes away, it is case 2.2. It is still worth an issue when rakudo survives the same
    unsynchronized script, because that is a compatibility gap rather than a safety bug.

## 4. Alternatives rejected

- **Keep the stricter promise and build the #12814 option (a)** (copy-on-write of every
  cross-thread-reachable node plus an atomic publish). It is sound, but it costs a rewrite of the
  write routes and contended-`push` throughput, and it protects only programs that are already
  wrong on every Raku implementation.
- **Keep the sentence but treat crashes as low priority.** That leaves an invariant the code does
  not meet, and every audit would have to re-derive that it is not really one.
- **Drop memory-safety guarantees under threads entirely.** It would also give up guarantees 1-3,
  which rakudo does give (MoarVM is memory-safe for synchronized programs and for its own internal
  structures). Following rakudo means keeping them.
