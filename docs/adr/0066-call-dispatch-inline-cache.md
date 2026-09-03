# ADR-0066: Call dispatch should resolve through a per-callsite inline cache, not a name-keyed hash map

- Status: **Accepted** (implemented 2026-09-03)
- Date: 2026-09-03
- Related: [ADR-0004](0004-jit-strategy.md) (J4d light-call caches),
  [ADR-0006](0006-baseline-interpreter-optimizations.md) (baseline interpreter
  optimizations), `todo/perf/late-august-call-path-slowdown-remainder.md`

## Context

`exec_call_func_op` is the VM's function-call dispatcher. After the
2026-09-03 round of call-path fixes (#7259, #7261, #7262, #7265, #7266, #7267,
#7268, #7269 — cumulatively about −38% on `benchmarks/fib.raku`), it is the
**second-largest self-time symbol** in a `bench-fib` profile, at 14.1%, behind
only `call_compiled_function_positional_light_at` at 36.3%.

`perf annotate` shows where that 14.1% goes. It is not the dispatcher's own
logic — it is **two hash-map probes per call**, both visible as hashbrown's
SIMD group scan (`movdqa` / `pcmpeqb` / `pmovmskb`, the single hottest
instruction inside the function at 13.1% of its self time):

1. `self.pos_light_call_cache.get(&name_sym)` — the name-keyed light-call
   cache, `FxHashMap<Symbol, PosLightTarget>`.
2. `compiled_fns.get(&key)` — resolving the cached key to the callee body.

`CompiledFns` is `FxHashMap<Symbol, CompiledFunction>` and `CompiledFunction`
embeds a whole `CompiledCode` by value, so it is **2440 bytes** (`imul $0x988`
appears in the generated probe as the bucket stride). That is a real smell —
a rehash memcpys kilobytes per entry — but see "What step 1 actually measured"
below before assuming it is what costs time here.

Both probes exist to answer a question that is **fixed per call site** in the
overwhelmingly common case. `fib`'s recursive call site resolves to the same
`CompiledFunction` on all 242 785 executions, and re-derives it by hashing a
symbol twice each time.

This is the classic case for a *monomorphic inline cache*: cache the resolved
target **at the call site**, not in a global map keyed by name.

## Decision

Resolve a repeat call from a cache the dispatcher can read without hashing,
instead of from two name-keyed hash probes.

The mechanics proposed below were superseded during implementation — the
per-callsite addressing they describe was built, measured, and found no faster
than the probes it replaced. **Read "What was actually built" for the shipped
design**; this section is kept because the reasoning that follows it (and the
"Why the existing fingerprint check cannot simply be dropped" section) is what
the shipped design had to satisfy.

Original proposal — add a per-callsite inline cache to the call opcodes:

1. `OpCode::CallFunc` (and `CallFuncNamed`, and later `CallMethod`) gains a
   `cache_idx: u32`, assigned by the compiler — one slot per call site in a
   `CompiledCode`. `OpCode` has budget: it is currently within the 48-byte cap
   the `opcode_size_guard` test pins, and `CallFunc`'s payload is
   `{u32, u32, Option<u32>}`.
2. `CompiledCode` gains a side table of cache slots, filled lazily.
   `CompiledCode` is shared behind `Arc` across threads, so the slots must be
   `Sync`: an `AtomicU64`-based slot, or a `Box<[OnceLock<...>]>` **plus** an
   invalidation generation. Precedent exists in the same struct — `const_syms`
   is already a lazily-filled `OnceLock<Box<[OnceLock<Symbol>]>>` side table
   addressed by a constant index.
3. A slot holds the resolved target plus everything needed to prove it is
   still valid: the `fn_resolve_gen` it was filled at, the callsite package
   symbol (the OTF cache already keys on this), and the callee's
   `fingerprint`. A miss or a failed validation falls back to today's path and
   refills the slot.
4. The cached target must be a *handle*, not a raw pointer into the map's
   storage (see Alternatives).

## What step 1 actually measured — and the reasoning it corrected

The first draft of this ADR proposed an easy first step: change `CompiledFns`
to `FxHashMap<Symbol, Arc<CompiledFunction>>`, on the theory that 2440-byte
inline values made "every probe step walk kilobytes". **That theory was
wrong**, and the change was prototyped, measured and then dropped rather than
shipped.

*Why the theory was wrong.* hashbrown is a SwissTable: the group scan reads the
**control-byte array**, which is contiguous and independent of the value type.
`perf annotate` on the current build shows exactly that shape —
`movdqu (ctrl)` / `pcmpeqb` / `pmovmskb` / `tzcnt`, and only then the
`imul $0x988` that computes the matched bucket's address. The value size
therefore costs one cache line on the *matched* bucket, not on the scan. What
is expensive is **the two probes themselves** — two hash computations and two
group scans per call — which is precisely what an inline cache removes and
what a value-type change cannot touch.

*What it measured.* Interleaved A/B of two release builds, nine alternating
runs each, both orderings (every sign flipped):

| benchmark | cycles | instructions |
| --- | ---: | ---: |
| `fib` | −3.5% | |
| `bench-fib` | −3.1% | −0.2% |
| `bench-tak` | −1.5% | |
| `bench-class` | +0.7% | |
| `method-call` | **+2.1%** | |

Retired instructions barely move (−0.2%), confirming no work was removed: the
small win is pure locality on the matched bucket, and it is paid back on
`method-call`, where the extra pointer hop to reach `CompiledFunction`'s fields
costs more than the smaller table saves. A mixed result with a disproven
rationale is not worth shipping, so it was not.

*What survives from the experiment.* Two things worth knowing:

- The migration is **small**: 12 compile errors, all mechanical
  (`insert(k, Arc::new(cf))`, `.map(|cf| &**cf)`, `std::ptr::eq(&**func, cf)`).
  Whoever implements the inline cache and needs an `Arc` handle to store can
  do it in an hour.
- `vm_call_named_inner.rs`'s multi-dispatch arm currently writes
  `Arc::new(compiled.clone())` — a **deep clone of the whole 2440-byte
  `CompiledFunction`, `CompiledCode` included**, on every call that takes it.
  Arc-valued `CompiledFns` turns that into a refcount bump. That site was not
  hot in any benchmark measured here, but it is a genuine per-call kilobyte
  copy on the multi path and deserves its own look.

If a value-type change is revisited, the better shape is probably
`Box<CompiledCode>` *inside* `CompiledFunction` — it shrinks the map's value to
a few hundred bytes while keeping the fields the dispatcher reads next
(`fingerprint`, `param_defs`, `param_local_slots`) inline, which is what the
`Arc` version gave up. That is a hypothesis, not a measurement.

## Why the existing fingerprint check cannot simply be dropped

It is tempting to read `PosLightTarget::Compiled { key, fingerprint }` as "a
key plus a redundant sanity check" and conclude that holding the resolved body
directly would be equivalent. It would not be, and this is the constraint the
inline-cache design has to respect.

`exec_call_func_op` receives `compiled_fns` **as a parameter**. Different
compilation units — an `EVAL`, a module body, a class body — run against
*different* `CompiledFns` maps, while `pos_light_call_cache` is keyed only by
the callee's `Symbol`. A cache entry filled while running one unit can
therefore be consulted while running another, and the fingerprint re-check
against the *current* map is what stops it from handing back the wrong unit's
body. `fn_resolve_gen` does not cover this: it tracks registration changes, not
which map is in hand.

A per-callsite cache does not have the problem at all — the slot lives in the
`CompiledCode` that owns the call site, so the unit is implicit in the
addressing. That is an argument *for* the inline cache over any attempt to
strengthen the name-keyed one, and it is the reason step 4 of the decision says
the cached target must be a handle plus enough identity to prove it belongs
here.

*As built, the constraint is discharged more directly than by either route:*
the table itself now carries a version token, so the slot proves it is holding
the right unit's body by comparing one `u64` rather than by re-deriving the body
from a key. See "What was actually built" below. The reasoning above still
explains why the check cannot simply be deleted — only what replaced it
changed.

## Alternatives considered

- **Do nothing.** The two probes are ~14% of a call-dominated benchmark and
  scale with every call in every program. Rejected.
- **Interpreter-side cache keyed by `(code_ptr, cache_idx)`.** Rejected: that
  is itself a hash lookup, which is the cost being removed.
- **Cache a raw `*const CompiledFunction` into `compiled_fns`.** Rejected: any
  insertion that grows or rehashes the map invalidates every such pointer,
  including insertions for unrelated names (`require`, `EVAL`, module load,
  class-body method registration all bump `fn_resolve_gen`, but a map mutation
  need not). An `Arc` handle has no such hazard.
- **Polymorphic (N-way) inline caches.** Out of scope. Start monomorphic; a
  megamorphic site simply keeps falling back to the current path, which is
  what it does today anyway.

## What was actually built

The decision's *goal* stands — the two hash probes are gone from a repeat call —
but the structure that replaced them is not the per-callsite one this ADR
proposed, because that structure was built, measured, and found to be no faster
than the probes. Both the shipped design and the discarded one are recorded
here.

**1. Per-callsite addressing was built first, and it did not pay.** A call site
was identified by its opcode index (which both the interpreter dispatch arm and
the JIT's `call_func` shim already have, so no `OpCode` field and no compiler
change was needed), a lazily-built `OnceLock` side table on `CompiledCode`
mapped that index to a slot number, and the slots lived in a `Vec` on the
`Interpreter`. It worked — on `benchmarks/fib.raku` the name-keyed cache was
consulted twice in 242,785 calls and the inline cache answered the rest — and it
**removed no retired instructions at all** (+0.07%).

The reason is the one thing the ADR did not account for: *what the two probes
cost is their dependent loads*, not their instruction count. A SwissTable probe
is only about 15 instructions. Reaching a per-callsite slot through
`OnceLock` → side table → index array → slot vector → slot is five dependent
loads, which is what two probes cost. Trading a hash for an equally long pointer
chase buys nothing.

**2. What shipped is direct-mapped and inline in the interpreter.** The cache is
a fixed 128-way `[CallIcSlot; N]` array *embedded in the `Interpreter`*, indexed
by `name_sym.id() & (N - 1)`. A lookup is one masked load of a 32-byte entry —
one dependent load — and four integer comparisons against tokens that all live
in that same entry. Nothing is per-chunk, so `CompiledCode`, `OpCode`, the
compiler and the opcode-index threading are all untouched. Colliding names
evict each other, which costs a miss and nothing else.

The ADR rejected an "interpreter-side cache" on the grounds that it "is itself a
hash lookup". A direct-mapped array is not: there is no hashing, no probe
sequence, and no bucket comparison — the mask *is* the lookup.

**3. Validity is a version token on the table, not an `Arc` handle.** The ADR
rejected caching a raw `*const CompiledFunction` because "any insertion that
grows or rehashes the map invalidates every such pointer". That objection is
answered by making the invalidation observable: `CompiledFns` is now a newtype
over the map carrying an `id` drawn from a process-global counter and **re-drawn
on every mutation**. Ids are never reused, so a single `u64` comparison proves
both halves of what the pointer needs — that the table in hand is the very table
the address came from, and that it has not been mutated since. That is strictly
stronger than the fingerprint re-check it replaces (which proved neither), and
it costs a compare instead of a probe. `id() == 0` marks a table that has never
been mutated — the state of the several empty scratch tables the dispatch paths
build — and can never validate an entry.

An entry therefore carries four tokens: the epoch (below), the table id, the
callee name, and the package the call ran under.

**4. The name-keyed cache it memoises got an epoch.** `pos_light_call_cache` is
still filled by the slow path; the dispatch cache is a memo of its answer, valid
exactly while that cache has not moved. One counter, bumped on every insert and
on the generation clear, expresses that — so the new cache adds no invalidation
surface of its own. Everything that already retired the name-keyed cache
(`fn_resolve_gen`, and thus `require`, module load, `EVAL`, class-body
registration, `wrap`) retires the dispatch cache with it.

## The bug this uncovered: the name-keyed caches were package-blind

Writing the ADR's own adversarial case — "a call site reached from two different
packages" — found a live, pre-existing wrong answer:

```raku
module PkgA { our sub which() { 'A' }; our sub probe() { which() } }
module PkgB { our sub which() { 'B' }; our sub probe() { which() } }
say PkgB::probe();  # B
say PkgA::probe();  # B  -- rakudo says A
```

`which` is a *different routine* in each package, and the full resolver
(`resolve_function_with_types`) knows that — it reads `current_package`. But
three caches in front of it did not: `fn_resolve_cache` (keyed by name, arity
and argument types), `light_call_cache` and `pos_light_call_cache` (keyed by
name). Whichever package called a given bare name first therefore answered for
every other package, in both directions, for the rest of the run. All three are
now keyed by `(name, callsite package)`, which is the key their *contents*
already assumed — `PosLightTarget::Otf` had been carrying a `callsite_package`
field and checking it by hand, and that field is now redundant with the key and
gone. Pinned by `t/call-inline-cache.t`.

## Measured

Retired instructions are the primary result. Cycles measured **across two
builds** cannot support a conclusion at this effect size on this box: the same
source built twice differs by more than the change is worth (the shipped
binary's own IC-disabled run of `fib` was 151.3 Mcycles against the baseline
binary's 141.9 — 6.6% apart with identical semantics, and a cross-build A/B of
this change reported anywhere from −1.5% to +3.1% depending on which pair of
builds was compared, `codegen-units=1` included).

So the cycle figures below come from a **same-binary** A/B: one build carrying a
temporary `MUTSU_CALL_IC=0` switch that skips the cache lookup, alternated
against itself. That holds codegen, inlining and layout exactly fixed and leaves
only the cache's own effect. (The switch was removed before merge.) Nine
alternating pairs, P-core pinned, medians:

| benchmark | instructions | cycles |
| --- | ---: | ---: |
| `fib` | −2.8% | −2.9% |
| `bench-fib` | −2.8% | −2.5% |
| `bench-tak` | −1.5% | −1.4% |
| `method-call` | ~0 | ~0 |
| `bench-class` | ~0 | ~0 |

Instructions and cycles move together, which is the shape to expect from work
that is simply removed. `method-call` and `bench-class` are unmoved because they
dispatch through the method path, which this cache does not serve yet.

Within the dispatcher, `exec_call_func_op` falls from 15.9% to 7.1% of a
`bench-fib` profile.

## Still open

- The **named** light-call path (`light_call_cache`) and `CallMethod` still pay
  two probes each. `CallMethod` is why `method-call` and `bench-class` show
  nothing here, and is the obvious next consumer of the same table.
- The cache is monomorphic per name. A megamorphic name evicts itself and falls
  back to today's path, which is what it does now anyway.

## Consequences

- Cache validity is a correctness surface. Every mechanism that can change what
  a name resolves to must invalidate: `fn_resolve_gen` covers
  registration/`require`/module load/`wrap`, and the table's `id` covers a body
  swap or a different compilation unit. Anything that changes resolution
  *without* touching either would be a live bug — the ADR's main risk, and the
  reason the validation plan below is not optional.
- `CompiledFns` no longer implements `DerefMut`. Every mutating entry point
  lives on the newtype and re-draws the `id`; adding one that does not would
  silently leave stale addresses validating. There are few (`insert`, `retain`,
  `extend`) and they are all compile-time paths.
- The dispatch cache holds raw addresses into a table it does not own, read
  through `unsafe`. The invariant is documented at both the `CompiledFns` type
  and the read site, and it is a *single* condition — `id` equality — rather
  than a set of conditions a future change could partially break.
- The shipped design needs nothing from the compiler or from `OpCode`, so the
  original consequences about threading `cache_idx` and sizing a per-chunk side
  table do not apply.

## Validation plan

- `make test` + `make roast` are the correctness gate, as always.
- Deliberately adversarial cases to add as `t/` pins: a call site reached from
  two different packages; a sub redefined by `EVAL` between two calls of the
  same site; a `require`d module replacing a name mid-run; a multi candidate
  set growing after the first call; a `wrap`/`unwrap` around a cached callee.
- Measure with retired **instructions** as well as cycles
  (`perf stat -e cpu_core/instructions/u`, interleaved A/B, both orderings) —
  the layout lottery is ~5% on this box and cycles alone cannot discharge it.
