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

  - [x] **Slice 4c (part 1) — hoist the immutable dynamic/magic-var defaults into
        the base tier.** Done (the "natural extension of 4b" below).

        A per-call env probe after 4b showed the ~49-entry overlay still carried
        ~20 *immutable* process-constant magic/dynamic vars: `$*VM`/`*VM`/`?VM`,
        `*PERL`/`?PERL`, `*RAKU`/`?RAKU`, `*KERNEL`/`?KERNEL`, `*DISTRO`/`?DISTRO`,
        `$*EXECUTABLE`(`-NAME`), `$*SPEC`, `*PID`, `*TZ`, `*INIT-INSTANT`. These are
        set once at interpreter start and never reassigned/removed by normal
        programs, yet every per-frame env clone+fork copied all of them.

        **What shipped.** `Interpreter::new` now moves this fixed allowlist
        (`IMMUTABLE_BASE_DYNAMICS`) out of `self.env` and into the shared
        [`GLOBAL_BASE`] tier (alongside the 4b enum constants) before
        `set_global_base`. Reads fall back to the base via `Env::get`/`get_sym`; a
        rare write is promoted into the overlay by `Env::get_mut`; a `my $*VM`
        shadow writes the overlay and reverts on scope exit. The base stays
        invisible to `iter`/`keys`/`len`/`remove`, so the block-exit writeback scan,
        `clone_for_thread`'s env iteration, and the handle-id scan are all
        unaffected (none of these vars are handles or mutable dynamics).
        *Mutable* dynamics (`$*OUT`, `$*ERR`, `$*IN`, `$*CWD`, `$*TMPDIR`, `$*HOME`,
        `%*ENV`, `@*ARGS`, `$*SCHEDULER`, `$*REPO`, `$*ARGFILES`) intentionally stay
        in the overlay.

        **Result.** The per-call deep copy now forks a ~29-entry overlay instead of
        ~49 (the *count* is still unchanged — that needs part 2). Release benches
        (best-of-5, interleaved): **read-only closure 1.30s→1.11s (~1.17x),
        mutating closure 2.15s→1.66s (~1.30x), 30k read-only closure 9.44s→8.07s
        (~1.17x); `bench-class` neutral** (its methods take the
        `skip_env_setup` light path, which already avoids the big-env fork). Bench
        output byte-identical between baseline and change. `make test` failure set
        unchanged from the `main` baseline (`placeholder.t` t8, `tail-function.t`
        t4, `wrap.t`, `hyper-func-op-writeback.t` t8 all pre-existing). New pin
        `t/base-tier-magic-vars.t` (read / underscore alias / sub / block / closure
        / shadow / thread visibility).

  - [x] **Slice 4c (part 2a) — kill the per-read/per-call `format!` on the hot
        variable-read and closure-call paths.** Done.

        **Measurement first (the deep-copy count was a red herring at this scale).**
        A `perf` profile of a closure-heavy bench showed `Env::cow_mut` (the
        `make_mut` deep copy) is now negligible; the dominant runtime cost is
        per-op string/format/Symbol churn. Two hot `format!` sources:
        1. **`exec_get_global` / the `GetLocal` shared-read path run an atomic-var
           check on *every* variable read**: a `format!("__mutsu_atomic_name::{n}")`
           plus *two* `var_type_constraint` calls (each itself a
           `format!("__mutsu_type::{n}")` + env lookup). Closures read their free
           vars via `GetGlobal`, and recursion (`fib`) reads its param constantly,
           so this is on the critical path of essentially all variable-read-heavy
           code — yet atomics are exotic.
        2. **The closure capture-state persist/load** keyed state by
           `format!("__mutsu_closure_cap::{id}::{name}")` (String) in the shared
           `state_vars` map, allocating + string-hashing per free var per call.

        **What shipped.**
        * A monotonic `Interpreter::atomic_var_seen` flag, set when any `atomicint`
          constraint is registered (`set_var_type_constraint`) or atomic storage is
          created (`atomic_value_key_for_name`), and inherited across thread clones
          (atomics are shared via `shared_vars`). The two hot atomic-read checks are
          gated behind it, so a program with no atomics skips the whole check
          (`format!` + constraint lookups) on every read. All three trigger
          conditions of the check are covered by the two flag-set sites, so the gate
          can never hide a real atomic.
        * A dedicated typed store `closure_captured_state: HashMap<(u64, Symbol),
          Value>` replaces the formatted-String `__mutsu_closure_cap::` keys,
          removing the per-call `format!` + String hashing from closure capture
          persistence.

        **Result (release, best-of-7, interleaved).** **`bench-fib` 1.56s→1.23s
        (~21%)** (every recursive call reads its param), **read-only closure
        8.05s→7.06s (~12%)**, **mutating closure 15.20s→14.18s (~7%)**, `bench-class`
        ~3%. Bench output byte-identical. `make test` failure set unchanged from the
        `main` baseline (`placeholder.t` t8, `tail-function.t` t4, `wrap.t`,
        `hyper-func-op-writeback.t` t8). Atomic semantics verified: whitelisted
        `roast/S17-lowlevel/atomic.t` + `atomic-ops.t` (62 subtests) green; new pin
        `t/atomic-read-gate.t` (ordinary/closure reads + atomic inc/fetch/assign +
        cross-thread counter). (`roast/S17-lowlevel/lock.t` fails identically on the
        pre-change binary — pre-existing, not whitelisted.)

  - [x] **Slice 4c (part 2b) — Symbol-key the closure exit writeback scan.** Done.

        **The deep-copy count turned out not to be the prize.** A `perf` profile of
        a mutating-closure bench on the post-part2a binary put `Env::cow_mut` (the
        `make_mut` deep copy) at only **~2 %**. The dominant cost is **Symbol→string
        churn (~33 %: `Symbol::starts_with` / `with_str` / `PartialEq<&str>` /
        `intern`)** plus residual `format!` (~14 %), concentrated in the closure
        **exit writeback scan** (`call_compiled_closure`). So neither removing the
        per-call fork nor the merge loop is where the time is.

        **Negative result recorded — the captured-env *fallback layer* does not pay
        off.** I prototyped giving `Env` an `Arc<Env>` fallback tier so closure
        dispatch could expose `SubData::env` by reference instead of the
        `entry_or_insert_sym` merge loop (reads fall through; iteration stays
        overlay-only, so the writeback semantics are provably unchanged — verified
        green incl. the advent tests). But Slice 4b already hoisted ~110 constants
        out of the env, so the captured overlay is now only **~30 entries**, not
        ~100: the merge is cheap, while the fallback adds a per-call
        `Arc::new(data.env.clone())` allocation + a read indirection. Measured net:
        read-only closure ~5 % faster but **mutating closure ~3 % slower** — not
        worth the blast radius to `Env`. Abandoned. (Lesson: post-4b the dual-store
        per-call cost is small; the lever moved to the writeback machinery.)

        **What shipped instead.** The exit writeback scan built its
        param/local/rw-source membership sets as `HashSet<&str>`/`HashSet<String>`
        and did a `with_str` (Symbol→&str resolve) on *every* working-env entry for
        each set, plus `*k != "_"` / `*k != "@_"` `PartialEq<&str>` compares. These
        are now `HashSet<Symbol>` (names interned once per call) and precomputed
        sentinel `Symbol`s, so the per-entry checks compare interned `Symbol`s
        directly (u32 + hash) with no string resolution. The scan only runs for
        non-leaf / mutating closures (read-only leaf closures already skip it), so
        this targets exactly the mutating-closure path.

        **Result (release, best-of-9, interleaved): mutating closure 13.71s→12.43s
        (~9.3 %)**; read-only closure, `bench-class`, `bench-fib` neutral (they do
        not run the writeback scan). Output byte-identical. `make test` failure set
        unchanged from the `main` baseline; 68 whitelisted S04/S06/S32-list/
        S32-hash/S03-metaops files + closure pins + advent integration tests all
        green.

  - [x] **Slice 4c (part 2c) — gate the per-call sigilless/state metadata
        write-back scans behind a monotonic flag.** Done.

        **What was costing.** After part 2b the closure exit path still ran, on
        *every* call, three per-free-var / per-env scans that almost always find
        nothing: (1) a `format!("__mutsu_sigilless_readonly::{n}")` +
        `format!("__mutsu_sigilless_alias::{n}")` lookup per free var, (2)
        `merge_sigilless_alias_writes`, which scans the whole working env **twice**
        doing `Symbol::starts_with` on every key, and (3) a
        `format!("__mutsu_state_key::{n}")` lookup per free var. A program with no
        sigilless variables and no state variables (the common case, incl. the
        read-only `map`/`grep` blocks) created none of those metadata keys yet paid
        all three scans. The profile put `format!`/`fmt::write` at ~11 % and
        `Symbol::starts_with` at ~8.5 %.

        **The robustness problem the doc flagged, solved at the choke point.** The
        ~20 metadata-creation sites are scattered across 5+ files, so a per-site
        monotonic gate (like `atomic_var_seen`) was error-prone. But *every* such
        key is created via the String-keyed `Env::insert` (always a `format!`
        result) — `insert_sym` is never used for them (verified). So a single
        prefix check in `Env::insert` catches all creation sites regardless of
        caller. A process-global monotonic `AtomicBool` (`env.rs
        CLOSURE_META_KEY_SEEN`) is set there for `__mutsu_sigilless_*` /
        `__mutsu_state_key::*` / `__mutsu_predictive_seq_iter::*`; the closure exit
        path reads `closure_meta_keys_possible()` once and skips all three scans
        (and the two residual `starts_with` prefix checks in the main writeback
        loop) when it is false. The flag is global + monotonic, so an over-set only
        ever makes the (correct) scan run — never wrong — and a program's metadata
        lives in its own per-thread env created earlier in that thread's program
        order, so `Relaxed` ordering suffices. Also switched the writeback scan's
        `local_names` set from re-interning `cc.locals` per call to the
        compile-time-precomputed `cc.locals_sym`.

        **Result (release, best-of-7, interleaved): read-only closure 7.21s→5.79s
        (~19.7 %), mutating closure 12.55s→10.55s (~15.9 %)**; `bench-fib` and
        `bench-class` neutral (within noise). Bench output byte-identical.
        Focused S03-binding/S04-blocks/S04-declarations(state)/S02-names/S06
        roast set (9 files) + closure pins + the three advent integration tests
        green. New pin `t/closure-meta-writeback-gate.t` (plain / read-only /
        state-var-per-instance / monotonic-flag / forwarded-call-mutation).

- [ ] **Slice 5 — stop flushing locals the callee can't observe (the dual-store
      collapse proper).** Designed; not yet implemented. This is the structural
      lever the earlier slices only optimized *around*.

      **The waste, measured.** `bench-fib` does **0 % function/method fallback**
      yet records **one `env_flush` per function-call opcode** (635593 flushes for
      635593 calls). The flush is `ensure_env_synced`: before dispatching a call it
      mirrors the caller's dirty simple-locals / bare-params into the shared `env`
      so a *callee that reads the caller scope by name* (interpreter fallback,
      closure, reflective access) sees current values. For a purely compiled callee
      like `fib` that binds its own params and reads them via `GetLocal`, the
      caller's `$n` flush is pure waste — the callee never reads the caller's `$n`
      from `env`.

      **Why it can't just be gated on `needs_env_sync`.** `needs_env_sync[i]`
      (Slice 4a) is exactly "local `i` is read via a `GetGlobal`-family op in this
      code, or captured by a nested closure." That covers GetGlobal reads and
      closures, but **not** the reflective readers that go through the interpreter
      without a compiled op: `EVAL` (verified: `sub f($n){ EVAL("\$n+1") }` reads
      `$n` from the shared env), symbolic deref `::($name)`, and
      `CALLER::`/`OUTER::`/`DYNAMIC::`. Those read arbitrary lexical names by
      string. So gating the flush on `needs_env_sync` alone would silently feed
      stale values to reflective code.

      **The design.** All reflective readers reach the env *through an interpreter
      fallback* (`call_function`/`call_method_*`/`run_instance_method` — the 18
      sites the per-name diagnostic counts). So:
        1. Gate the *pre-dispatch* flush (the `ensure_env_synced` calls that run
           before attempting compiled dispatch) on `needs_env_sync[i]`, so a
           compiled call no longer mirrors locals the callee can't name.
        2. Do a *full* `sync_env_from_locals` immediately before each interpreter
           fallback, so reflective callees still see a current env.
      Net: `fib` (compiled path only) stops flushing → ~0 flushes; reflective code
      gets a full sync exactly when it needs one.

      **Why it is correctness-critical and must be staged.** `exec_call_func_op`
      alone has *five* dispatch tiers (positional-light cache, named-light cache,
      OTF cache, the `arity<=1` fast-call path, then `dispatch_func_call_inner`),
      each with its own early `ensure_env_synced` and/or `return`. Method dispatch
      and `vm_var_get_ops` add more. The full flush must be placed before **every**
      fallback exit; a single missed site is a silent stale-read bug in reflective
      code. So implement per-path in small slices (start with the function-dispatch
      path that `fib` actually takes — pin which tier that is first — leaving method
      dispatch full-flush/unchanged), validate each with `make test` + the
      reflective roast tests (`EVAL`, `S02-names/symbolic-deref.t`,
      `S02-names/caller.t`, dynamic-scope) + full CI roast, and report the
      `env_flush` count drop per slice.

Each slice must keep `make test` + `make roast` green and report perf
(`method-call`, `bench-class`, `bench-fib`) before/after.
