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

The second probe is worse than an ordinary one. `CompiledFns` is
`FxHashMap<Symbol, CompiledFunction>` and `CompiledFunction` is **2440 bytes**
(`imul $0x988` appears in the generated probe as the bucket stride), so the
table stores 2.4 KB values inline: every probe step touches a fresh cache line
region, and every rehash memcpys kilobytes per entry.

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

## Step 1, independent of the above: `CompiledFns` values behind `Arc`

Changing `CompiledFns` to `FxHashMap<Symbol, Arc<CompiledFunction>>` is a
smaller, self-contained change that:

- makes probe (2) touch 8-byte values instead of 2440-byte ones, and makes
  rehashing cheap;
- lets `PosLightTarget::Compiled` hold an `Arc<CompiledFunction>` directly —
  exactly what `PosLightTarget::Otf` already does — which **eliminates probe
  (2) entirely** on a cache hit, leaving one hash lookup per call instead of
  two.

That alone is expected to recover a large share of the 14.1%, and it is a
prerequisite for step 4 above (an inline cache needs a target it can hold
without pinning the map's memory). It needs no ADR of its own; it is recorded
here so the ordering is explicit. **Do it first, measure, and only then decide
whether the full inline cache is still worth its complexity.**

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
