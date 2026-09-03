# ADR-0066: Call dispatch should resolve through a per-callsite inline cache, not a name-keyed hash map

- Status: **Proposed**
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

## Decision (proposed)

Add a per-callsite inline cache to the call opcodes.

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

## Consequences

- The compiler must allocate and thread `cache_idx` through every call-opcode
  emission site, and `CompiledCode::finalize` must size the side table.
- Hand-built chunks (the ones that already skip `finalize`'s `locals_sym`
  pre-interning) must degrade gracefully — an absent or short table means "no
  cache", exactly as `const_sym` already handles a short slot table.
- Cache validity becomes a correctness surface. Every mechanism that can
  change what a name resolves to must invalidate: `fn_resolve_gen` covers
  registration/`require`/module load today, and the fingerprint check covers
  a same-key body swap. Anything that changes resolution *without* touching
  either would be a live bug — the ADR's main risk, and the reason the
  validation plan below is not optional.

## Validation plan

- `make test` + `make roast` are the correctness gate, as always.
- Deliberately adversarial cases to add as `t/` pins: a call site reached from
  two different packages; a sub redefined by `EVAL` between two calls of the
  same site; a `require`d module replacing a name mid-run; a multi candidate
  set growing after the first call; a `wrap`/`unwrap` around a cached callee.
- Measure with retired **instructions** as well as cycles
  (`perf stat -e cpu_core/instructions/u`, interleaved A/B, both orderings) —
  the layout lottery is ~5% on this box and cycles alone cannot discharge it.
