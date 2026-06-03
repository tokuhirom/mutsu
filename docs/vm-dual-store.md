# Collapsing the VM's `locals` ↔ `env` dual store

Tracking design for the highest-leverage VM-decoupling task
(`ANALYSIS.md` §1.2, `PLAN.md` "🔴 最優先"). This is a high-blast-radius change,
so it is staged into small, individually-shippable slices. This file is the
map; record each slice's before/after here.

## The two stores today

| store | shape | who reads/writes it |
|---|---|---|
| `VM::locals` | `Vec<Value>`, slot-indexed | `GetLocal`/`SetLocal` fast paths (hot path) |
| `Interpreter::env` | `HashMap<String, Value>`, name-keyed | everything that crosses into the interpreter |

The VM keeps both in sync with three routines and three flags
(`src/vm/vm_env_helpers.rs`):

- `ensure_locals_synced` — env → locals, only when `env_dirty`.
- `ensure_env_synced` — locals → env, dirty-slot-aware, only when `locals_dirty`.
- `sync_env_from_locals` — locals → env, **unconditional full flush** (6 callers).
- flags: `env_dirty`, `locals_dirty`, `locals_dirty_slots: Vec<bool>`.

Each `VmCallFrame` snapshots eight env/dirty/bind fields, and
`push_call_frame` does a **full `clone_env()` on every compiled call** — this is
the "env deep clone ~9μs/call" cost in `PLAN.md`.

## Why a single store is hard: the env consumers

`env` cannot simply be deleted because these consumers read/write by name and
need a name-keyed, scope-shared view. Each is a constraint on the migration:

1. **Interpreter execution fallbacks** — `call_function` / `call_method_with_values`
   read args and globals from `env` by name (the bulk of the ~1300 refs).
2. **Closures** — captured as `GetGlobal`/`GetArrayVar`/`GetHashVar` against `env`
   (no upvalues; `ANALYSIS.md` §1.3). `compute_needs_env_sync` conservatively
   forces whole-frame env writeback when any closure exists.
3. **Bare-word / class-name resolution** — consults `env` for `my`-declared names.
4. **Dynamic variables** (`$*foo`) and `$CALLER::`/`$OUTER::` — dynamic scope via env.
5. **`:=` binding aliases** — `GetLocal` calls `resolve_binding` then reads `env`
   by the bound name (`exec_get_local_op`, vm_var_assign_ops.rs:3921).
6. **Atomics / CAS** (`atomicint`, atomic arrays) — authoritative value lives in
   `env` / shared-var store so cross-thread reads see updates
   (vm_var_assign_ops.rs:3930–3964).
7. **Shared vars across threads** — `clone_for_thread` shares a subset via
   `shared_vars`; `env` is the per-thread materialization.
8. **Attributes** (`!attr`) — managed via locals, but CAS writes go through env.

Consequences: locals are already authoritative for *reads* of simple scalars in
the common case (`ensure_locals_synced` early-returns when `!env_dirty`), but
**not** for bound / atomic / shared / attribute names, which intentionally treat
`env` (or the shared-var store) as the source of truth.

## Target architecture

Slot-indexed `locals` become the source of truth for ordinary lexicals. `env`
becomes a **lazily-materialized name view** built only when a slow path
(interpreter fallback, closure, dynamic lookup) actually needs it — not eagerly
cloned per call. Special names (bound/atomic/shared/attribute) keep an explicit
shared cell (`ContainerRef` / shared-var store) so they need no per-call sync.

Reaching that requires, roughly in order:
1. Give closures real upvalues so they stop reading parent `env` (removes the
   `compute_needs_env_sync` whole-frame writeback — `ANALYSIS.md` §1.3).
2. Make interpreter fallbacks take locals by slot (or a thin view) instead of a
   cloned `env`.
3. Replace the per-call `clone_env()` with copy-on-write / frame-local overlay.

## Slices (record results here)

- [ ] **Slice 0 (this doc).** Document the stores, consumers, and target. No code.
- [x] **Slice 1 — measure the sync cost.** Opt-in `MUTSU_VM_STATS` counters for
      `clone_env()`, **actual env deep copies** (`Arc::make_mut` on a shared env),
      `ensure_env_synced` flushes/slots, and `sync_locals_from_env` pulls (no
      behavior change).

      **Correction (important):** `Env` is *already* copy-on-write
      (`Arc<HashMap<Symbol, Value>>`, `src/env.rs`), so `clone_env()` is an O(1)
      Arc bump — **not** the real cost. The real cost is the O(env_size)
      `Arc::make_mut` deep copy triggered the first time a method body writes the
      env while its frame still holds a clone of it. Measured:

      | program | clone_env (O(1)) | **env_deep_copies (O(env))** |
      |---|---|---|
      | `fib(25)` (242785 user-sub calls) | 0 | **0** |
      | 5000× `P.new(x=>$_).g` (method calls) | 5000 | **15002 (~3 / call)** |
      | 2000× `C.new(n=>$_).calc` (writes locals) | 2000 | **6002 (~3 / call)** |

      So `bench-class` (2.3× raku, `PLAN.md`) pays **~3 full env-HashMap deep
      copies per method call**, while recursive **functions** pay **0** — the
      function light paths (`call_compiled_function_light` /
      `call_compiled_function_positional_light`) bind params to slots and never
      share-then-mutate the env. The lever is to stop method bodies from writing
      the env (bind `self`/params/locals to slots), which removes the `make_mut`
      deep copies.

- [x] **Slice 2 — skip the dispatch type-check's per-arg env bind.** Done.

      A temporary backtrace on the deep-copy site (gated `Backtrace` in
      `Env::cow_mut`) showed the per-call deep copies do **not** come from the
      method-frame env setup writes (skipping those changed the count by zero).
      They come from **method *resolution* / dispatch type-checking**:
      `resolve_method_with_owner_invocant → method_args_match →
      args_match_param_types → bind_param_value → env.insert`. To evaluate type
      matches, `args_match_param_types` snapshots the env (`saved_env =
      self.env.clone()`, O(1) Arc bump) then binds every arg into it — and the
      first bind `make_mut`-deep-copies the whole ~110-entry env. It runs ~3-4×
      per call (once per candidate / resolution phase).

      **Fix:** the outer per-arg `bind_param_value` only exists so a *later*
      param's `where {...}` / sub-signature / code-signature can reference
      earlier params by name (each such check already does its own local env
      snapshot+bind+restore). When no param has such a constraint
      (`needs_outer_bind == false`), the binds are skipped; the env
      snapshot/restore is kept unconditionally so coercion/subset checks still
      roll back. `method g{42}` / `$!x*2` dropped from **4 → 1 env deep copy per
      call** (20001 → 5001 over 5000 calls); `where`/sub-sig methods still bind.

      **Validation note (process lesson):** an earlier identical change was
      *mistakenly reverted* after attributing `t/wrap.t` / `t/placeholder.t`
      failures to it. Those three tests (`wrap.t`, `placeholder.t`,
      `tail-function.t`) in fact **fail on `origin/main` too**, in *release*
      builds, run individually and via full `prove t/` — they are pre-existing,
      not caused by this change. The optimized build's `make test` failure set is
      identical to main's baseline (plus the flaky concurrency test `t/lock.t`,
      which passes 5/5 in isolation). Lesson: when `make test` shows failures,
      diff the failure *set* against a clean build of the same revision before
      blaming a change — `main` does not currently pass `make test` cleanly in a
      local release build.

      **Remaining ~1 deep copy/call (located, deferred):** a follow-up backtrace
      shows the last per-call deep copy is the method **setup env writes** in
      `call_compiled_method_fast` (`vm_method_dispatch.rs` ~814/832: `self`,
      `?CLASS`, params, attrs). `push_light_call_frame` shares the env Arc, then
      the *first* of these inserts `make_mut`-deep-copies it; the rest are
      in-place. The body reads these from slots (they are populated into
      `self.locals` right after), so the writes exist only for interpreter
      fallbacks *within* the body. This one can't be cut by partial removal
      (any single remaining write still triggers the one fork) and full removal
      risks breaking in-method interpreter fallbacks that read `self`/`?CLASS`/
      params by name. The clean fix is the deeper one: stop snapshotting the
      whole ~110-entry flat env per call — i.e. a scoped/overlay env (the
      `locals`↔`env` collapse itself, Slice 3+) — so a per-call fork is O(scope)
      not O(global env). Deferred to that work rather than a risky local hack.

      Side facts: `can_skip_merge = !has_rw_params && !cc.has_env_writes`, and
      `has_env_writes` is set by **any** `CallFunc`/`CallMethod` in the body
      (`opcode.rs:1175`), so most non-leaf methods are not `can_skip_merge`.
      (Pre-existing, unrelated: `t/tail-function.t` test 4 fails in *debug*
      builds on `main` too — a release-only PredictiveIterator path.)
- [x] **Slice 3 — shrink closure capture-state work to free variables.** Done.

      **Measurement first (the headline was *not* deep copies).** A closure-heavy
      micro-benchmark (5000× a factory sub that builds a closure and calls it 20×,
      = 105000 closure calls) runs in **~15 s** and `MUTSU_VM_STATS` reports ~2
      `env_deep_copies` per closure call. But a `perf` profile showed `Env::cow_mut`
      (the deep copy) is only **1.3 %** of runtime. The real cost is the
      **per-call O(captured-env) work**: every closure call iterates the *entire*
      captured env (~110 entries, because `MakeBlockClosure` captures
      `self.env.clone()` = the whole outer env) in several loops —
      `cap_overrides`, the state persist loop, the readonly/alias writeback, and
      the state-key sync — each doing a `format!("__mutsu_closure_cap::{id}::{k}")`
      (or similar) per entry. Profile hotspots: `Symbol::intern` 9.3 %, hashing
      ~14 %, `format!`/`Display` ~10 %. ~550 `format!` allocations **per closure
      call**.

      **Backtraces** (gated `MUTSU_DEEPCOPY_BT` in `Env::cow_mut`) further showed
      the residual deep copies come from the `&?BLOCK` setup insert and the
      param-binding env write — both feeding cross-cutting interpreter features
      (`last`/`next` target resolution, `&?ROUTINE`, regex `$/`) that read
      `__mutsu_callable_id` / `&?BLOCK` from `env` *by name* even when the body
      text never mentions them. So those env writes **cannot** be made conditional
      or removed piecemeal — confirming the deep-copy count itself only falls with
      the full scoped/overlay-env refactor (Slice 4+).

      **What shipped instead — restrict the per-call loops to free variables.**
      Only a closure's *free* variables (names its body, or a nested closure's
      body, references from an enclosing scope) can be mutated by it, so only those
      carry per-instance captured state. `CompiledCode` now precomputes
      `free_var_syms` (GetGlobal-family operand names not in its own locals, unioned
      with nested closures' already-computed `free_var_syms`). `call_compiled_closure`
      iterates `cc.free_var_syms` (a handful of names) instead of `data.env.keys()`
      (~110) for `cap_overrides`, the state persist loop, the readonly/alias
      writeback, and the state-key sync. The captured-env *merge* keeps copying the
      full env (a foreign-scope caller may need any captured lexical for an
      interpreter fallback) but now keys by the interned `Symbol` directly
      (`Env::entry_or_insert_sym`) instead of `resolve()`+re-intern. The exit
      locals→env flush now writes back only captured locals (frame-local closure
      locals are discarded on env restore anyway).

      Capture semantics are unchanged: `data.env` still holds the full snapshot
      for fallback reads; only the *state-persistence* loops are narrowed, which is
      sound because non-free captured names can't be mutated by the body.

      **Result:** closure-heavy bench **~15 s → ~5 s (read-only closure, ~3×)** and
      **~15 s → ~6.5 s (mutating closure, ~2.3×)**, output identical to `raku`.
      `env_deep_copies` is unchanged (expected — this slice removes O(env) hashing/
      formatting, not the make_mut copies). `make test` failure set unchanged from
      the `main` baseline (`t/wrap.t`, `t/placeholder.t`, `t/tail-function.t`
      pre-existing). New regression pin: `t/closure-captured-state.t`.

- [ ] **Slice 4 — closure upvalues / scoped env.** Capture free variables into an
      explicit upvalue list (or a scoped overlay env) so closures stop reading the
      parent `env` by name, the `&?BLOCK` / `__mutsu_callable_id` setup writes move
      off the shared global env, and the conservative whole-frame `needs_env_sync`
      (`fill(true)` whenever any closure exists) can be dropped. This is what
      actually removes the per-call `make_mut` deep copies.

Each slice must keep `make test` + `make roast` green and report perf
(`method-call`, `bench-class`, `bench-fib`) before/after.
