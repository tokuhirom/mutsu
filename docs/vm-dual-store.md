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

      **Follow-up — skip the full-env writeback scan for read-only closures.**
      A re-profile after the above showed the new top hotspot (~45 %) was the
      *other* per-call O(env) loop: the exit writeback scans the whole working
      env (~110 entries) to propagate the closure's mutations back to the caller
      (`Symbol::starts_with`/`with_str`/`contains` on every entry). But a closure
      can only change outer state through a **free variable** — directly, or
      transitively via a nested closure that captured it from this frame and
      wrote it back; either way the variable's value in this frame's env differs
      from entry. So `call_compiled_closure` now snapshots its free vars' values
      before the body and skips the entire writeback scan when none changed (and
      `env_dirty` is clear, no rw params, no captured locals). The free-var-value
      diff is immune to the per-statement `?LINE` bookkeeping write that always
      dirties the env Arc (so a naive env-pointer-identity check is *not* usable —
      it always reports "changed"). It also correctly catches transitive
      grandparent mutation because a nested closure's free vars are folded into
      this code's `free_var_syms`. **Read-only closure ~5 s → ~2.7 s, mutating
      ~6.5 s → ~4.6 s** (cumulative from the original ~15 s: **~5.6× / ~3.2×**).
      Validated with `make test` (same pre-existing failures) and 193 closure/
      block/routine/sort/gather/sigilless roast files (3937 subtests) green.

      **Soundness fix — the skip is only valid for a *leaf* closure.** The
      original "free var changed" reasoning above is incomplete: it assumes the
      only outward mutation a closure can make is to its own free variables. But
      once the body makes a **call**, a nested method/closure that closes over an
      enclosing lexical can write *any* captured variable back into this frame's
      env — including one that is captured here yet is **not** a free variable of
      this closure, so the `free_changed` diff never sees it. The regressing case
      was `roast/integration/advent2011-day03.t`'s `capture-out`:
      `{ $*OUT.write($buf) }` mutates the enclosing `$output` through the
      dynamically-dispatched `$*OUT.write` method (a separate closure over
      `$output`), and that change must reach the caller. `advent2009-day07.t` and
      `advent2012-day20.t` failed the same way. Fix: gate the skip additionally on
      `!cc.has_env_writes`, which is set for any call / env-writing op in the body
      — so only a genuinely leaf, read-only closure skips the scan. The hot
      map/grep/sort blocks (`{ $_ * 2 }`, `{ $_ > 3 }`) contain no calls, so they
      still skip; bench numbers above are unchanged (read-only ~2.4 s, mutating
      ~4.4 s release). The three advent integration tests are green again.

- [ ] **Slice 4 — closure upvalues / scoped env.** Capture free variables into an
      explicit upvalue list (or a scoped overlay env) so closures stop reading the
      parent `env` by name, the `&?BLOCK` / `__mutsu_callable_id` setup writes move
      off the shared global env, and the conservative whole-frame `needs_env_sync`
      (`fill(true)` whenever any closure exists) can be dropped. This is what
      actually removes the per-call `make_mut` deep copies.

  - [x] **Slice 4a — drop the conservative whole-frame `needs_env_sync`.** Done.

        `compute_needs_env_sync` previously did `needs_env_sync.fill(true)` (mirror
        *every* local of the frame into `env`) the moment any closure existed in
        the frame, "because closures may capture any outer variable via GetGlobal."
        Since Slice 3 each `CompiledCode` knows its exact `free_var_syms` (the names
        it, or a nested closure, reads from an enclosing scope). A local of *this*
        frame is observable by a closure created here **iff** its name is in some
        nested closure's `free_var_syms` — that is precisely the set
        `ensure_env_synced` must flush before `MakeBlockClosure` snapshots the env.
        So the `fill(true)` is replaced by marking only those locals (unioned with
        the existing GetGlobal-family-in-this-code set). Frames with many locals but
        few captured ones now mirror only the captured handful instead of all.

        Soundness: a closure can only read an outer local by name through a compiled
        GetGlobal-family op in its (or a nested closure's) body, which is exactly
        what `compute_free_vars` collects; interpreter fallbacks inside the body read
        from the full captured-env snapshot, and special names (`&?BLOCK`, `$/`,
        dynamics) are not parent-frame locals. Verified that even `{ EVAL '$x' }`
        capturing a slot-local `$x` from an enclosing sub still resolves correctly.

        Perf: closure benches unchanged within noise (read-only ~3.0s, mutating
        ~5.0s release — the bench closures capture nearly all their parent's
        locals, so `fill(true)` and the precise set coincide; the win is in
        wide-frame code). No deep-copy count change (that needs 4b/4c). `make test`
        failure set unchanged from the `main` baseline (`t/placeholder.t`,
        `t/tail-function.t`, `t/wrap.t` pre-existing); S04/S06 (157 files) and
        S02/S12/S14 (241 files) whitelist green. New pin:
        `t/closure-selective-env-sync.t`.

  - [x] **Slice 4b — split the env into an immutable shared base tier.** Done.

        **Measurement first.** A per-call env probe on the closure bench showed the
        env holds **119 entries, of which ~110 are immutable built-in constants** —
        the `Order` / `Endian` / `ProtocolFamily` / `Signal` enum variants (bare +
        qualified, ~70), plus dynamic/magic var defaults — and only ~9 are real
        program lexicals (`base`, `factor`, `sum`, `_`, `@_`, ...). Each compiled
        call clones the env and the first setup write `Arc::make_mut`-deep-copies
        the whole 119-entry map (`env_deep_copies` ~2/call). So the dominant cost is
        copying ~110 constants that never change.

        **What shipped.** `Env` gains a process-wide immutable base tier
        (`env.rs::GLOBAL_BASE: OnceLock<HashMap<Symbol,Value>>`). The four
        `init_*_enum` functions now write their ~70 constant entries into that base
        (installed once at `Interpreter::new`) instead of `self.env`. `Env::get` /
        `contains_key` fall back to the base on an overlay miss; `get_mut` promotes a
        base key into the overlay before handing out `&mut` (so in-place mutation of
        a constant isn't lost); `flatten` merges base under overlay. Crucially
        `iter` / `keys` / `values` / `len` / `remove` stay **overlay-only** — the
        base is a name-lookup constant pool, not part of the mutable lexical
        environment, so it is invisible to the closure writeback scan and every
        other env iteration (which only ever look for lexicals/`__mutsu_*` keys,
        never enum constants). User-defined enums / `my` shadowing still work: they
        write to the overlay, which shadows base on read and reverts when the scope's
        overlay entry is removed.

        **Result.** The per-call deep copy now forks a ~49-entry overlay instead of
        119 (~2.4x cheaper per copy; the *count* is unchanged — that needs 4c).
        Release bench: **read-only closure 2.50s→1.33s (~1.9x), mutating
        4.38s→2.00s (~2.2x), method-call 0.71s→0.52s (~1.4x).** `make test` failure
        set unchanged from the `main` baseline (`placeholder.t`, `tail-function.t`,
        and `hyper-func-op-writeback.t` test 8 all fail on a clean `main` build too —
        the last is a pre-existing locale/version-sensitive failure, not a base-tier
        regression). 791-file enum/closure/integration whitelist green. New pin
        `t/builtin-enum-base-tier.t` (bare/qualified/closure/thread/shadow).

  - [ ] **Slice 4c — upvalue capture + move setup writes off the shared env.**
        Still to do: give closures an explicit upvalue list (or scoped overlay env)
        and move the `&?BLOCK` / `__mutsu_callable_id` / param-binding setup writes
        off the shared global env. This removes the per-call `make_mut` deep copies
        *entirely* (Slice 3 located them at exactly those setup writes; Slice 4b only
        shrank each copy, it did not cut the count). A natural extension of 4b is to
        also hoist the remaining immutable dynamic/magic-var defaults into the base
        tier so the overlay shrinks further.

Each slice must keep `make test` + `make roast` green and report perf
(`method-call`, `bench-class`, `bench-fib`) before/after.
