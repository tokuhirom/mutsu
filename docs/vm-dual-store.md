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
      `clone_env()`, `ensure_env_synced` flushes/slots, `sync_locals_from_env`
      pulls (no behavior change). **Baseline / key finding:**

      | program | clone_env | env_flushes | locals_pulls |
      |---|---|---|---|
      | `fib(25)` (242785 user-sub calls) | **0** | 0 | 0 |
      | 5000× `P.new(x=>$_).g` (method calls) | **5000** | 0 | 5001 |
      | `roast/6.d/S32-str/sprintf.t` | 0 | 0 | 8 |

      The per-call `clone_env()` cost is **localized to method calls** (and
      `:=`-binding function calls that use `push_call_frame`/`push_light_call_frame`).
      Plain/recursive **function** calls already avoid it — they go through the
      light call paths (`call_compiled_function_light` /
      `call_compiled_function_positional_light`) that bind params to slots without
      cloning env. So `bench-class` (2.3× raku, `PLAN.md`) pays one full env clone
      per method dispatch, while `fib` (1.0×) pays none.

- [ ] **Slice 2 — remove `clone_env()` from the method-call frame.** The
      function light path (`call_compiled_function_positional_light`,
      vm_call_dispatch.rs:732) already shows the pattern: instead of
      `clone_env()` (O(env_size)), it `mem::take`s `self.locals` and saves only
      the env entries for the *function's own local names* (`saved_env_locals`,
      O(locals)), restoring those on return. Apply the same selective
      save/restore to the method frame.

      Method-frame push sites to convert: `vm_method_dispatch.rs:140`
      (`push_call_frame`) / `:744` (`push_light_call_frame`); also
      `vm_closure_dispatch.rs:168` and `vm_call_dispatch.rs:1386`.

      **Risk / verify first:** `pop_call_frame` returns `saved_env` and some
      callers use it "for site-specific merge logic" — the full clone may be
      load-bearing for restoring env mutations a method made to *non-local* names
      (dynamic vars, attribute writeback, `$_`). Audit every `pop_call_frame()`
      caller's use of `frame.saved_env` before removing the clone. Guard with the
      full OO / closure / dynamic-var roast suites + `bench-class` (target:
      method clone_env → ~0, bench-class 2.3× → <2×).
- [ ] **Slice 3 — closure upvalues.** Capture free variables explicitly so
      closures stop reading parent `env`; drop the conservative whole-frame
      `needs_env_sync`.

Each slice must keep `make test` + `make roast` green and report perf
(`method-call`, `bench-class`, `bench-fib`) before/after.
