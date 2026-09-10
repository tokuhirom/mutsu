# `Symbol::flags()` is a lock-free global load, not a thread-local `RefCell` round trip

`Symbol::flags()` is the memoized flags word the hot merge/dispatch paths ask
about a name (`PLAIN_USER_LEXICAL`, `ATTR_TWIGIL_ENV_KEY`, `CALLABLE_ID_META`,
...). It was memoized in a thread-local `RefCell<Vec<u16>>`, so a *hit* — which
is essentially every call — still cost **33 Ir**: a thread-local access, a
`RefCell::borrow` (two writes to the borrow counter plus the guard's drop), a
bounds-checked `Vec` index and a copy, all to answer a pure function of a `u32`.

That is hot because `capture_closure_env`'s filter asks it about every visible
env key on every closure creation — 100% of the calls in both profiles below.

## What changed

The memo is now a process-global, lock-free chunked table of `AtomicU16`
(`FLAG_TABLE` in `src/symbol.rs`), and it is primed at **intern** time:
`intern_global` is the one place a symbol id is ever assigned, so it computes and
stores the flags word as it hands the id out. `Symbol::flags()` is therefore a
chunk-pointer load plus a relaxed 16-bit load, and the compute path is a `#[cold]`
fallback reached only for an id whose store has not landed yet.

Nothing about the memo's validity is new: interned ids are global and append-only
(never reused, never remapped) — the same invariant `symbol::wk` already relies on
to cache well-known symbols in a `OnceLock` for the life of the process. That is
what makes the memo shareable across threads rather than per-thread, and what
makes `RefCell` unnecessary: each entry is written once, with the same value
whoever writes it, and a reader that races a writer sees the not-yet-`COMPUTED`
zero and recomputes the identical word. Reads and writes of one entry are atomic
operations on the same location, so they are coherent without any ordering
stronger than `Relaxed`.

`compute_flags` remains the single source of truth and the bits keep the
`env::is_*` predicates they mirror, so no semantics changed. Storage is chunked
so nothing is preallocated: the static costs one pointer slot per chunk, a chunk
is allocated the first time an id lands in it, and an id past the table's reach
degrades to an unmemoized recompute rather than an out-of-bounds index.

## Measurement

Callgrind, release build, same machine, before and after:

| program | total Ir before | total Ir after | delta |
|---|---|---|---|
| `sub make($n) { my $c = { $n + 1 } }` x 20000 | 1,087,465,807 | 1,072,764,417 | **-14,701,390 (-1.35%)** |
| `benchmarks/bench-ctor.raku` | 1,428,665,097 | 1,426,740,432 | -1,924,665 (-0.13%) |

In the closure microbenchmark, `Symbol::flags` (21,783,249 Ir, 2.00%) disappears
from the profile entirely: the remaining body is small enough that LLVM inlines
it into its callers. Taking `capture_closure_env`'s filter closure and
`Symbol::flags` together — the filter is where every call comes from — the pair
goes from 53,443,249 Ir to 40,120,000 Ir over 600,000 keys, about **22 Ir saved
per key**.

## Regression cover

`src/symbol.rs`'s tests pin that the memo answers exactly what a fresh
`compute_flags` scan would for a spread of name shapes, that a symbol interned on
one thread reports the same flags on another (the point of going global), that
`intern_global` primes the slot so the first `flags()` for a new name is already a
pure read, and that an id beyond the table falls back instead of panicking.

Closes #7856.
