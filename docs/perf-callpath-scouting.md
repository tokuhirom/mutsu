# Perf scouting: what actually costs time on the call path

Entry point for the next perf session. This is a **scouting document**: measured
profiles, the root cause behind each hot symbol, and the slices they imply — in
the order they should be attempted. No design is committed here beyond what the
data supports; every slice carries its own gate.

The measurement discipline this document follows is
[ADR-0006 §「実装スライスの計測プロトコル」](adr/0006-baseline-interpreter-optimizations.md)
(judge with `perf stat -e instructions:u` under `taskset -c <p-core>`; wall-clock
and kernel-inclusive `instructions` swing 2–8% on this hybrid CPU and cannot
decide a 3% change).

> **2026-07-14 rewrite.** The first version of this document (written at #4489)
> named the *per-call env overlay* as the prize and pointed at the lexical-scope
> slot campaign. That call was **wrong, and the measurements below disprove it** —
> see §4. What actually cost the time was per-call and per-assignment **string
> formatting and hashing**, which #4492–#4495 removed for gains of 24–39%. The
> corrected model is recorded here so the next session does not re-derive it.

## 1. What landed (2026-07-13/14) and what it bought

| PR | change | effect |
|---|---|---|
| #4492 | `readonly_vars` was a `HashSet<String>` **deep-cloned on every call** (`RawTable` alloc + a heap `String` per entry + SipHash). Now `Arc<FxHashSet<Symbol>>`, saved by `Arc` bump. Also removed the per-call `HashSet<&str>` built by the return merge, and the re-interning local-seed loop. | bench-fib **−32.3%**, bench-tak **−23.9%** |
| #4493 | Every scalar store built two env keys with `format!("__mutsu_sigilless_{alias,readonly}::{name}")` + `Symbol::intern`. Now pre-interned per local slot (`CompiledCode::locals_alias_sym` / `locals_readonly_sym`). Also dropped a duplicated `flush_local_to_env`. | mandelbrot −14.9%, num-arith −21.6% |
| #4494 | Instance attributes were `HashMap<String, Value>`. Now `AttrMap` = `FxHashMap<Symbol, Value>`, with the hot `$!x`/`$.x` paths resolving through a per-chunk `local_attr_keys` table (no twigil parse, no interning, no `format!`). | method-call **−11.5%**, bench-class −2.7% |
| #4495 | A `my $t = ...` declaration ran **six `format!`s and four env probes before storing anything** (speculative clears of an earlier same-named variable's metadata). Every scalar store also called `reset_atomic_var_key` → `format!` + env probe + a `shared_vars` **read lock**. All pre-interned or gated. | mandelbrot **−33.7%** (cumulative), time-parts **−37.2%**, num-arith **−32.1%** |

Cumulative vs. the pre-campaign `main`: time-parts −37%, mandelbrot −34%, fib
−32%, num-arith −32%, tak −24%, method-call −15%, hash −8%, string −6%, class −4%.

**The lesson generalizes:** the interpreter's hot paths were paying for
*name-keyed metadata* — building `__mutsu_*::<name>` strings at runtime and
hashing them. Every remaining `format!("__mutsu_…::{name}")` on a per-op path is
a candidate; the fix is always the same (pre-intern the `Symbol` per local slot,
plus a monotonic "was such a key ever created" gate).

## 2. The profile now (post-#4495, release, JIT on, one P-core)

### bench-tak — the next target (mutsu 0.36s vs raku 0.20s, **1.8×**)

| symbol | % | what it really is |
|---|---|---|
| `call_compiled_function_positional_light` | 12.5 | the light-call frame itself |
| `_int_malloc` + `malloc` + `_int_free` + `cfree` | **28.4** | per-call allocation churn |
| **SipHash `Hasher::write`** (+ `hash_one` ×2 ≈ 4.6) | **6.7** | see §3.1 — the `compiled_fns` lookup |
| `exec_call_func_op` | 3.2 | dispatch |
| `Env::scoped_child` | 2.9 | per-call env overlay |
| `arg_is_container_value` | 2.9 | per-arg call-eligibility scan |
| `indexed_varref_from_value` | 2.7 | per-arg signature binding |
| `hashbrown::HashMap::insert` | 2.6 | |

### bench-mandelbrot (assignment-dominated)

Now that the `format!`s are gone, what is left is `exec_set_local_op_inner`
itself (~11%), the allocator, and `Env::get_sym`/`memcmp`/SipHash from the
remaining `String`-keyed side maps (`var_type_constraints`, `var_defaults`).

## 3. The slices, in order

### 3.1 `compiled_fns` is a `HashMap<String, CompiledFunction>` with the default hasher — **do this first**

`src/vm/vm_call_func_ops.rs` — the function table is threaded through every
dispatch as `&HashMap<String, CompiledFunction>` (**std `HashMap` = SipHash**).
Even the *ultra-fast* positional light-call path does, per call:

```rust
if let Some((cached_key, cached_fp)) = self.pos_light_call_cache.get(&name_sym)   // FxHashMap<Symbol, (String, u64)>
    && let Some(cf) = compiled_fns.get(cached_key.as_str())                       // ← SipHash over the name + memcmp
```

So a monomorphic call that hits the cache *still* hashes the function name with
SipHash and memcmps it. That is the 6.7% `Hasher::write` on tak (and part of the
`memcmp`). The cache exists precisely to avoid resolution, yet it re-does a
string hash to get back to the callee.

- **S1a (mechanical)**: make the table `FxHashMap`. A type alias
  (`type CompiledFns = rustc_hash::FxHashMap<String, CompiledFunction>`) plus a
  sweep of ~84 occurrences across 38 files. Kills the SipHash; keeps the memcmp.
- **S1b (better)**: key the table by `Symbol`. `Symbol` is `Copy` and already
  interned per constant slot (`CompiledCode::const_sym`), so hashing becomes a
  `u32` and the memcmp disappears.
- **S1c (best)**: have the light-call caches store what the callee *is* (an
  index / `Arc<CompiledFunction>`) instead of a key to look it up with — a cache
  hit then does **zero** map lookups.

Gate: `instructions:u` on bench-tak / bench-fib / method-call / poly-call.

### 3.2 Per-call allocation churn (28% of tak)

The single biggest line on tak, and #4492 only took the `readonly_vars` share of
it. Profile the remaining `malloc` callers with a DWARF call-graph before
guessing: candidates are `Env::scoped_child`'s `Arc<SymMap>` box, the args `Vec`,
`take_locals_from_pool` misses, and the `Value` clones in argument binding.

### 3.3 The remaining `String`-keyed side maps

`var_type_constraints: HashMap<String, String>` and `var_defaults:
HashMap<String, Value>` are probed by name on every assignment (they short-circuit
when empty, so they cost nothing for programs that use neither — but any typed
variable turns them on). Same treatment as #4493/#4494: key by `Symbol`.

### 3.4 `Value::eq` on instances is super-linear — a correctness cliff, not just perf

Found while landing #4494. `Value::eq` deep-walks an instance's attribute map with
**no cycle guard and no memo**, and `AttrReadGuard::new`/`drop` do O(depth)
thread-local `Vec` scans. So *any* deep-equality compare on a long or recursive
object graph is super-linear.

`cas` was the only caller the suite exercised that way, and it was only fast **by
accident**: it used a structural `new_val == current` pre-check, and with `String`
keys the SipHash iteration order happened to visit a cheap mismatching attribute
first and short-circuit. Symbol keys reordered the map, the walk went down a
4000-node linked list, and `roast/S17-lowlevel/cas-loop.t` went 2.7s → 53–80s.
Fixed in #4494 by using the identity predicate (`cas_retry_matches`) the compare
on the very next line already used. **The same cliff still sits under
`is-deeply`/`eqv`** — worth a dedicated slice.

## 4. Disproved: what is NOT the problem (do not re-derive this)

The previous version of this document claimed the prize was §2.2 "every call still
materializes a name-keyed env tier" and pointed at the lexical-scope slot
campaign. Three experiments (2026-07-14) say otherwise:

1. **The `captures_env_by_name` flush blanket costs nothing on the loop
   benchmarks.** `compute_needs_env_sync` marks *every* local of a frame
   containing a `ForLoop`/`BlockScope` as `needs_env_sync`, which looks like "any
   function with a `for` loop mirrors every local write into env". Removing
   `ForLoop` from that list and re-measuring: **±0% on every benchmark.** The
   reason is visible in `MUTSU_VM_STATS`: bench-mandelbrot reports
   **`env_flushes=0`** — it never goes through `flush_local_to_env` at all. The
   env write it *does* perform comes from the slow path's unconditional
   `set_env_with_main_alias`, which `needs_env_sync` does not gate.

2. **The SetLocal "fast path for simple scalar variables" is dead code.**
   `simple_locals` is computed as `name.starts_with('$') && …`, but a plain
   lexical scalar's local name is stored **sigil-stripped** (`my $x` → `"x"`). So
   `simple_locals` is false for every ordinary scalar and the fast path in
   `exec_set_local_op_inner` never runs. Widening the predicate does light it up
   (verified: `env_flushes` goes 0 → 322417 on mandelbrot) — **but it breaks**
   (`my $s = Supplier.new; $s.emit(1)` → "No such method 'emit' for invocant of
   type 'Any'"). The fast path was written against sigiled names and has never
   been exercised for scalars. Reviving it is its own slice with its own
   validation, not a free win.

3. **`exec_block_scope_op`'s whole-`locals` clone never appears in a profile.** It
   is real technical debt (see below), but it is not where the time is.

### What this means for the lexical-scope slot campaign

[docs/lexical-scope-slot-campaign.md](lexical-scope-slot-campaign.md)'s remaining
step (§1.3: drop the `exec_block_scope_op` whole-`locals` clone) should be
motivated as **architecture, not perf**. The coupling was measured precisely:

- `BlockScope` cannot leave the `needs_env_sync` blanket while
  `exec_block_scope_op` restores the whole `locals` array and re-seeds it from env
  by name — a block body's write to an *enclosing* local only survives the block
  through its env mirror. Removing the blanket for `BlockScope` alone deterministically
  breaks `FIRST/NEXT/LAST` loop phasers (`t/phasers.t` #3,
  `t/dualstore-slot-local-gate.t` #6): the `LAST` body's `$seq ~= "L"` is reverted
  at block exit.
- So the clone removal and the blanket narrowing are **one change**, in this order:
  bake a compile-time `block_declared_slots` onto the `BlockScope` opcode → replace
  the whole-array restore with a targeted reset of just those slots → then drop
  `BlockScope` from the flush blanket. (The loop-phaser control temps —
  `__mutsu_loop_first_*`, `__mutsu_loop_ran_*`, … — are threaded through env by
  name deliberately and must stay `needs_env_sync` regardless; they are recognisable
  by their `__mutsu_loop_` prefix.)

Do that work for the architecture (it removes the dual store), and expect the perf
payoff to be small.
