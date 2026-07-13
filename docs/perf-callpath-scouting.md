# Perf scouting: what actually costs time on the call path (2026-07-13, post-#4489)

Entry point for the next perf session. This is a **scouting document**: measured
profiles, the root cause behind each hot symbol, and the slices they imply — in
the order they should be attempted. No design is committed here beyond what the
data supports; every slice carries its own gate.

The measurement discipline this document follows is
[ADR-0006 §「実装スライスの計測プロトコル」](adr/0006-baseline-interpreter-optimizations.md)
(judge with `perf stat -e instructions:u` under `taskset -c <p-core>`; wall-clock
and kernel-inclusive `instructions` swing 2–8% on this hybrid CPU and cannot
decide a 3% change).

## 1. The profile

`perf record -F 999 -e cycles:u`, release build, **JIT on (the default config)**,
pinned to one P-core, `main` at #4489.

### bench-fib (call-dominated)

| symbol | % | what it really is |
|---|---|---|
| `call_compiled_function_positional_light` | 10.9 | the light-call frame itself |
| `_int_free` + `_int_malloc` | **11.8** | per-call allocation churn |
| `Env::scoped_child` | 5.4 | per-call env overlay |
| **JIT native code (`mutsu_jit_1`)** | *5.7* | ← the actual compiled loop |
| `exec_call_func_op` | 5.7 | dispatch + name lookup |
| `Env::get_sym` | 4.4 | name-keyed reads |
| **SipHash `Hasher::write`** | 4.1 | see §2.1 — `compiled_fns` lookup |
| `hashbrown RawTable::clone` | 3.7 | env overlay / map clones |
| `drop_in_place<Env>` | 2.0 | env teardown |
| `peek_callsite_line` | 1.7 | see §2.3 |

**The headline: the JIT-generated native code accounts for only 5.7% of fib.**
Everything around the call — allocation, hashing, env — dwarfs it. Making the
JIT better cannot fix this; the call path must get cheaper.

### bench-class (object-dominated)

| symbol | % | what it really is |
|---|---|---|
| `malloc` + `_int_malloc` + `_int_free` + `malloc_consolidate` | **19.5** | allocator |
| `nanbox::gc_op` + `Gc::drop` | 7.7 | refcount traffic |
| `__memcmp_avx2` | 5.2 | `String`-keyed attribute lookups |
| `exec_call_method_mut_op` | 2.7 | method dispatch |
| `AttrReadGuard::drop` | 2.3 | attribute access |

## 2. Root causes found by reading the code

### 2.1 `compiled_fns` is a `HashMap<String, CompiledFunction>` with the default hasher

`src/vm/vm_call_func_ops.rs:121` — the function table is threaded through every
dispatch as `&HashMap<String, CompiledFunction>` (**std `HashMap` = SipHash**).
Even the *ultra-fast* positional light-call path does, per call:

```rust
if let Some((cached_key, cached_fp)) = self.pos_light_call_cache.get(&name_sym)   // FxHashMap<Symbol, (String, u64)>
    && let Some(cf) = compiled_fns.get(cached_key.as_str())                       // ← SipHash over the name + memcmp
```

So a monomorphic call that hits the cache *still* hashes the function name with
SipHash and memcmps it. That is the 4.1% `Hasher::write` (and part of the
`memcmp`). The cache exists precisely to avoid resolution, yet it re-does a
string hash to get back to the callee.

Slices, cheapest first:

- **S1a (mechanical)**: make the function table `FxHashMap` instead of the
  default hasher. 84 occurrences of the type across 38 files — a type alias
  (`type CompiledFns = rustc_hash::FxHashMap<String, CompiledFunction>`) plus a
  sweep. Kills the SipHash; keeps the memcmp.
- **S1b (better)**: key the table by `Symbol` (`FxHashMap<Symbol, CompiledFunction>`).
  `Symbol` is `Copy` + already interned per constant slot (`CompiledCode::const_sym`,
  `opcode.rs:1949`), so hashing becomes a `u32` and the memcmp disappears.
- **S1c (best, if S1b is not enough)**: have the light-call caches store what the
  callee *is* (an index / `Arc<CompiledFunction>`) instead of a key to look it up
  with — a cache hit should then do **zero** map lookups.

Gate: `instructions:u` on bench-fib / method-call / poly-call. Expect S1a+S1b to
recover most of the 4.1% + a slice of the memcmp; if it does not, stop and
re-profile rather than escalating to S1c on faith.

### 2.2 Per-call env overlay + allocation churn (the real prize)

`Env::scoped_child` (5.4%) + `Env::get_sym` (4.4%) + `RawTable::clone` (3.7%) +
`drop_in_place<Env>` (2.0%) + a large share of the 11.8% malloc/free is one
thing: **every call still materializes a name-keyed env tier**, even for a body
whose locals are all slot-resolved. `src/env.rs` documents the COW/overlay design
(`scoped_child`, `MAX_OVERLAY_DEPTH` flatten) — it is already well optimized *as a
name-keyed map*; the win is to not need it per call.

This is the existing **lexical-scope slot campaign**
([docs/lexical-scope-slot-campaign.md](lexical-scope-slot-campaign.md)), whose
final step is removing `exec_block_scope_op`'s `self.locals.clone()`. It is the
load-bearing refactor (`$OUTER::` runtime snapshots, GC roots, env re-sync) and
deserves a dedicated session — but the profile says it is where the time is.

### 2.3 `peek_callsite_line` scans the argument list on every call (1.7%)

`src/runtime/call_helpers.rs:194` — every call walks its args looking for a
synthetic `TEST_CALLSITE_LINE_KEY` pair marker. With #4489 the source line is now
static per-instruction data (`CompiledCode::op_lines`), so the *marker itself* is
arguably obsolete for anything the VM can reach from `ip`: the Test-module
callsite line could be pulled from the line table at the call op instead of being
smuggled through the argument vector. Small, self-contained, and it also removes
an `args.retain()` on the NativeCall path.

### 2.4 Attributes are `HashMap<String, Value>` (bench-class)

`src/value/value_instance.rs:91` — attribute maps are `String`-keyed, hence the
5.2% `memcmp` and a large share of bench-class's ~20% allocator time (each key is
a heap `String`; `to_map()` clones). PLAN §5 already carries this as "attrs の
`HashMap<String,Value>`/SipHash 起因の malloc 群（＝作り直し本体）". `Symbol` keys
are the obvious move, and the same interning already exists.

## 3. Suggested order

1. **S1a/S1b — `compiled_fns` hasher + `Symbol` key.** Mechanical, bounded, and
   it removes a per-call string hash that the cache was supposed to make
   unnecessary. Good first slice: it validates the measurement protocol on a
   change whose expected effect is known.
2. **§2.3 — retire the callsite-line argument marker** now that the line table
   exists. Small and self-contained.
3. **§2.4 — `Symbol`-keyed attributes.** Medium; directly targets bench-class's
   allocator and memcmp time.
4. **§2.2 — the lexical-scope slot campaign** (`BlockScope` locals clone/restore
   removal). The big one; dedicated session.

Do **not** resume the opcode-histogram line (`SetVarDynamic`, `CheckReadOnly`)
without first showing, per the ADR-0006 protocol, that those opcodes cost time —
#4489 removed 21% of executed opcodes for -3.4% instructions precisely because
opcode *count* is not opcode *cost*.
