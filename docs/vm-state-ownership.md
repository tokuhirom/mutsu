# ③ Migrating execution state to VM ownership (interpreter-bridge removal — structural design)

**Step ③** of the [PLAN.md](../PLAN.md) final goal "remove the tree-walking Interpreter execution path → delete the dual-store".
Following ① (eliminating individual fallbacks) → ② (VM ownership of the declaration registries, #2760-2775, done), this is
**the biggest mountain**. This document is the finalized design for ③. Prerequisites: [vm-registry-ownership.md](vm-registry-ownership.md) (②),
the primary ledger [vm-interpreter-fallback-ledger.md](vm-interpreter-fallback-ledger.md),
and [vm-dual-store.md](vm-dual-store.md) (locals↔env, lever B).

## The state ③ must resolve

`VM` **owns `interpreter: Interpreter` by value** (`src/vm.rs:88`). Of the Interpreter state borrowed by
VM-native code, the **execution state** other than the declaration registries moved in ② (`Arc<RwLock<Registry>>`):

- **env** (the variable store itself, `Interpreter.env: Env`) — by far the hottest. `self.interpreter.env`/`env_mut` =
  **483 sites** in the VM tree alone.
- **Type checking**: `type_matches_value` (VM 8 + runtime 26 files), `var_type_constraint`/`var_type_constraints`,
  `var_hash_key_constraints`.
- **readonly tracking**: `readonly_vars`/`mark_readonly`/`unmark_readonly` (VM 4 + runtime 2).
- **let/temp restoration**: `let_saves`/`restore_let_saves`/`discard_let_saves` (VM 5 + runtime 2).
- **state variables**: `state_vars`/`our_vars`/`once_values` (VM 0 + runtime 3 — almost entirely on the tree-walk side).
- **`current_package`** (VM 8 + runtime 33).
- **multi resolution**: `resolve_function_with_types`/`has_multi_*`/`has_proto`.

Once these move to the VM, **the interpreter bridge itself becomes unnecessary**, and we can proceed to
④ (carrier finalization) → ⑤ (dual-store mechanism removal).

## Core point: ③ differs decisively from ② in method (the shared-handle approach cannot be used)

② lifted the registry onto an `Arc<RwLock<Registry>>` scaffold. We must NOT do the same to env in ③:

1. **env has no "true concurrent sharing".** env **exists in exactly one `Interpreter` object**, and
   ping-pongs (`Interpreter::run_block_raw` → `VM::new(self)` → `vm.run()` → `*self = interp`,
   `src/runtime/run.rs:956`) **by value between nested VMs**. Recursion like VM-native → `interpreter.call_function`
   → a new `VM::new` inside … also just **moves** the Interpreter each time, and the live env is
   always a single instance. Cross-thread use is a **snapshot** via `clone_for_thread` (no write-back).
   → ② needed `Arc<RwLock>` because "`Interpreter` crosses the thread boundary in `spawn_user_thread` and the registry
   is touched by multiple threads". **env has a single owner, so the end state is a plain VM field — full stop** —
   the Arc/lock scaffold is unnecessary.
2. **Making env `Arc<RwLock>` would wreck perf.** env is the hottest path of all variable reads/writes
   (483 sites + deep runtime). Even for ②'s registry, the transitional RwLock acquisition cost +2-4% on release microbench.
   Putting a lock on env per-access would be a regression of a different order of magnitude (`Env` is already an internal
   `Arc<HashMap>` COW, so layering an outer lock on top would also be pointless).

→ **③ cannot proceed by the "lift the field onto a shared handle" approach (②'s playbook).**

## Why "field relocation first" is also not an option — the forced strategy

So, can we directly "move env from Interpreter to a plain field on VM right now"? **No**:

- The high-value state is **pervasive in the tree-walk code** under `src/runtime/` (`type_matches_value` 26 files,
  `current_package` 33 files, env is effectively the entire runtime). These tree-walk methods **run user code via
  §1/§2 fallbacks during VM execution**, and read `self.env`/`self.type_matches_value`/
  `self.current_package` while doing so.
- Physically pulling the fields out of Interpreter would break all these thousands of `self.env` accesses. Threading env
  as an argument through the entire runtime tree-walk call graph is unrealistic.

→ Conclusion: **③ is driven by fallback eradication, not by field relocation.**
Replace the tree-walk execution paths of ledger §1/§2 (catch-all method dispatch / function dispatch / native-method)
with **VM-native implementations**; each eradication removes "the tree-walk readers of that state" one class at a time. Once the last
tree-walk execution path disappears, env/types/state/current_package no longer have readers in runtime/ and
**can be folded into plain VM fields** (this final fold is ④/⑤). **The slices of ③ = the rows of ledger §1/§2.**

## State × coupling map (survey finalized 2026-06-08)

| state | VM sites | runtime/ coupling | Independently movable? | Treatment in ③ |
|---|---|---|---|---|
| `env`/`env_mut` | 483 | everywhere | No (hottest, everywhere) | Fold last, after fallback eradication |
| `type_matches_value` | 8 | 26 files | No (deep in tree-walk) | Readers shrink with §1 eradication |
| `current_package` | 27 | 33 files | **Migrated via shared handle** (below) | Unlocks VM-side registry dispatch reads |
| `var_type_constraint(s)` | 33 | 8 | Hard | Together with type checking |
| `readonly_vars`/`mark_readonly` | 8 | 3 | **Relatively local** | Candidate for early move to VM |
| `let_saves`/`restore_let_saves` | 10 | 3 | **Relatively local** | Same (but low decoupling value while env remains) |
| `state_vars`/`our_vars`/`once_values` | 0 | 3 | runtime-exclusive | Follows tree-walk eradication |

Key point: **even if the local states (readonly/let) are moved to the VM first, the decoupling value is small
as long as env stays in Interpreter** (the Interpreter object does not go away). The value materializes the moment
the tree-walk execution paths disappear and env can be folded. Therefore investment concentrates on §1/§2 eradication.

## Ledger §1/§2 = the slices of ③ (eradication order)

The primary status is in [vm-interpreter-fallback-ledger.md](vm-interpreter-fallback-ledger.md). Remaining tree-walk:

- **§1 catch-all method dispatch** (largest): `interpreter.call_method_with_values` at `vm_call_method_compiled.rs:427/857`,
  `vm_call_method_mut_ops.rs:303` =
  the live Instance/Buf/Failure method fork. **The main remaining tree-walk.**
- **§1 native-method** (IO family): `native_io_*` requires interpreter-owned state such as file handles →
  predicated on state (handles) migration.
- **§2 function dispatch**: `vm_call_func_ops.rs` (builtin-shadow / multi-dispatch / final else),
  `vm_call_dispatch.rs` catch-all, `vm_dispatch_helpers.rs:356-384` function resolution of Routine values.
  **② (VM ownership of the registry) put the structural prerequisites in place** = this is the first realistic entry point for ③.

Mandatory eradication procedure (same as ②/dedup): **confirm equivalence via the EVAL route** that the native implementation is
authoritative (`mutsu -e 'say EVAL(q{...})'` matches raku), then remove the tree-walk, then verify with `make roast`.

## Slice plan (strangler-fig; each PR behavior-invariant; CI is the safety net)

> Order: "§2 function dispatch, whose structural prerequisites ② put in place" → "§1 catch-all method dispatch (the main stronghold)" →
> "native-method (together with IO state migration)" → "fold env/types/state into plain VM fields (④/⑤)".

- **PR-1 (VM-nativize §2 function/Routine dispatch)**: Move the Routine-value resolution in `vm_dispatch_helpers.rs` and
  the multi/sub resolution in `vm_call_func_ops.rs` to VM-native dispatch using the `Registry` methods that became
  VM-accessible in ② (`resolve_*` / `has_multi_*`). Reproduce the builtin priority order
  (when a Routine points at a builtin) on the VM side. The interpreter remains only as the terminal (EVAL/carrier).
- **PR-2 onward (§1 catch-all method dispatch)**: Route live Instance method calls to the unified
  `call_compiled_method_*` dispatch, executing all user-defined class methods as bytecode.
  Only native/reflective/MOP remain at the shared terminal (carrier). Move Buf/Failure native methods down into builtins/.
- **PR-n (native-method IO)**: Move `handles` to VM ownership and make `native_io_*` VM-native.
- **Final fold (④/⑤)**: Once the tree-walk execution paths are gone, move env/type checking/readonly/let/state/current_package
  from `Interpreter` to plain `VM` fields, remove the Interpreter object and the ping-pong,
  and delete `env_dirty`/`saved_env_dirty`/`ensure_locals_synced`/`sync_locals_from_env`.

## Scope / non-goals

- Scope = **VM-nativization of the tree-walk execution paths** (deleting the rows of ledger §1/§2). Each PR is behavior-invariant.
- Non-goals (later stages): folding env/state into plain fields (④/⑤, after Interpreter removal), final determination of the
  §C carriers (EVAL sub-VM / regex-embedded `{}` / pseudo-package), resolving the Arc-ptr keying of `type_metadata`
  (🟣 first-class containers), true cross-thread registry sharing (PLAN §8.3 concurrency).

## Progress

- **Design finalized (this document, 2026-06-08)**: Confirmed that ③, unlike ②, is **fallback-eradication-driven**
  (the shared-handle approach is not usable; env's end state is a plain field). Created the state×coupling map.
  Slice plan = eradication of ledger §1/§2. Next step = PR-1 (VM-nativization of §2 function/Routine dispatch, unlocked by ②).
- **PR-1 done (Routine dispatch, 2026-06-08)**: Consolidated the 3 raw `interpreter.call_function` calls
  (qualified / bare / terminal) in the Routine-value resolution of `vm_dispatch_helpers.rs::vm_call_on_value`
  into the unified entry `call_function_compiled_first`, executing user-defined sub/multi/proto as compiled bytecode.
  **Builtin priority preserved**: only Routines carrying a builtin name
  (`&SETTING::...::not` → `Routine{GLOBAL, "not"}`) keep `call_function`'s builtin priority via the `is_builtin_function`
  guard (a plain user `&not` is a `Value::Sub` and never reaches the Routine branch). The naive conversion regressed
  `S02-names/SETTING-6e.t` → reproduced and fixed. pin = `t/routine-value-dispatch.t`(10). Consumed the Routine row of ledger §2.
- **PR-2 done (builtin-shadow fork, 2026-06-08)**: In the function-dispatch fork where a user sub shadows a same-named builtin
  (`exec_call_func_op` + `exec_call_func_slip_op` in `vm_call_func_ops`, the `user_function_matches_call` branch),
  defs that are **plain single-candidate and compilable** are OTF-compiled via `compile_and_call_function_def` and run as bytecode
  (avoiding the shadowed builtin without falling into the native arm). The compilability check was consolidated into the
  `def_is_otf_compilable` helper.
  **Two rounds of regressions found and fixed**: ① proto/multi were also OTF-compiled as single candidates, breaking proto'd
  multi candidate dispatch (S06-multi/proto, subsignature, type-based aborted mid-file) → added the `!has_proto && !has_multi_candidates` guard.
  ② The `user_function_matches_call` branch is not builtin-shadow-only — it receives every "args-matching user sub absent from compiled_fns".
  `def_is_otf_compilable` failed to capture nested `sub`+`when` control flow and Test::Util `is-deeply-junction` broke →
  **restricted to actual builtin shadows with the `is_builtin_function` guard** (lesson: measure the *true reachable set* of a fallback branch; never assume it).
  pin `t/builtin-shadow-dispatch.t`(9). **Next: §1 catch-all method dispatch (the main stronghold) or the §2 non-proto multi fork.**
- **PR-3 done (§1 catch-all method dispatch — reality correction + compile-on-demand, 2026-06-09)**: Started on §1 catch-all
  as "the main stronghold = the main remaining tree-walk (user methods)", but **measuring the reachable set with `MUTSU_VM_STATS` +
  probes showed that premise to be wrong** (PR-2's lesson "measure the true reachable set, don't assume it" paid off here too). Reality:
  normally declared methods (including multi), submethods, role composition, inheritance, and `.^add_method` are **all
  bytecode-compiled at registration time by `compile_class_methods`** (`.^add_method` inherits the Sub literal's `compiled_code`).
  The Instance traffic reaching the catch-all is only native coercion (`Exception.Stringy`) / MOP (`.does`/`.^does`) / role-qualified
  (resolve=None), and these depend on ③ state ownership / pushing down into builtins / a separate resolution bug. **The only remaining
  compile gap is `.^add_multi_method`** (fixed at `compiled_code: None`). As the fix, added `populate_uncompiled_method`
  at the dispatch chokepoint (when a resolved def has `compiled_code==None`, idempotently compile via
  `compile_class_methods`/`compile_role_methods` → re-resolve → `dispatch_compiled_method`), reliably bytecode-compiling the
  user-method share (if the owner is a non-user class or the body is empty, the interpreter fallback is preserved).
  pin `t/method-otf-dispatch.t`(14). **Conclusion: what remains of §1 catch-all is native/MOP dispatch, and its eradication is
  really state-ownership migration (env/handles to VM) plus pushing native methods down into builtins**
  (routing alone will not remove it). The value of folding env materializes only after this native dispatch is VM-nativized.
  **Next: the §2 non-proto multi fork (VM-side multi candidate resolution), or pushing native methods down into builtins (Buf/Failure).**
- **PR-4 done (§2 non-proto multi fork, 2026-06-09)**: Extended the non-proto multi fork
  (`has_multi_candidates && !has_proto`) of `vm_call_func_ops.rs::dispatch_func_call_inner` to the PR-2 pattern:
  resolve the winner with `resolve_function_with_types` (VM-accessible since ②), and if `def_is_otf_compilable` and the body is
  state-free, OTF compile / bytecode-execute via `compile_and_call_function_def`. **Ambiguity on the function path is expressed as
  `None`+`pending_dispatch_error`** (`Some(def)`=unambiguous) — a separate mechanism from the method-side `dispatch_ambiguous`.
  Ambiguity/where/default/code-param/no-match/proto/terminal keep `call_function_fallback` (which throws the canonical exceptions).
  **nextsame/callsame/callwith work even from compiled candidates**
  (`compile_and_call_function_def` sets up the same `push_multi_dispatch_frame` — measurement confirmed no redispatch exclusion is needed).
  Pitfalls: ① name pollution of otf_call_cache (the type-blind cache wrongly reused differently-typed candidates) → added the
  `!has_multi_candidates_cached` guard on both insert and lookup. ② shared state across signature alternates (the state_group sharing of
  `(A)|(B){state $x}` splits under per-alternate fingerprints) → excluded state-carrying multi bodies from OTF via
  `function_body_declares_state` (found and fixed the `t/multi-signature-alternates.t` regression). Slip-path multis are out of scope due
  to a pre-existing separate bug (`|ms()`=Nil).
  pin `t/multi-otf-dispatch.t`(25). multi-probe fallback 5→2, S06/S12/S14 all green, make test PASS.
  **As a side effect, fixed an existing flake**: `push_multi_dispatch_frame` determined the current candidate for callsame by
  HashMap-order first match (when the narrowest candidate is declared later, a ~50% seed-dependent flake; the intermittent failures of
  whitelisted `callsame.t` etc. = the substance of CI #2788's failure).
  Fixed to identify it with the same deterministic winner as the interpreter inline frame (`resolve_function_with_alias`) =
  all VM multi paths made deterministic. Redispatch exclusion became unnecessary (callsame works correctly even with OTF candidates).
- **PR-5 done (§1 catch-all = plain `@`-array mutators, 2026-06-09)**: **Measured and corrected the reality** of §1 catch-all.
  Temporarily instrumented the type names of catch-all receivers and measured the reachable set over the whole whitelist sample:
  PR-3's estimate "what remains is native/Buf/Failure" was wrong — the actual dominant traffic is
  **array mutators (`Array.append`=16721, `Array.shift`=5796, `Array.splice`) + the iterator protocol
  (`pull-one`/`skip-one`/`push-exactly`) + coercion (`List.Set/Bag/Mix`)**. The largest, `Package.new`=38698, is the ③ constructor
  (user BUILD / attribute-default execution) and is ③-blocked. Started on the **largest tractable category excluding `Package.new` = array mutators**.
  Added `try_native_array_mut` to `vm_call_method_mut_ops.rs`, VM-nativizing **append/prepend/unshift/pop/shift on plain untyped `@`-arrays**
  (`env.get_mut`+`Arc::make_mut` — identical to the interpreter primary branch). typed/shaped/lazy/shared/metadata-carrying/
  non-env-bound conservatively keep the interpreter = behavior-invariant. The **Arc-ptr keying aliasing of type_metadata**
  (a known 🟣 hazard) was surfaced by `make_mut` reallocation, so `unregister_container_type_metadata` is defensively applied to
  native-path outputs (safe under the untyped guarantee).
  pin `t/native-array-mut.t`(31).
- **PR-6 done (measurement of the §2 catch-all terminal + pushing junction constructors down, 2026-06-09)**: Instrumented the
  `call_function_compiled_first` terminal (`vm_call_dispatch.rs:79`) with `END:` probes; the **terminal is nearly exhausted**
  (PR-1..4 plus full OTF compilation of resolved defs removed the high-traffic fallbacks). The remaining terminal traffic is diffuse and small =
  junction constructors / **name-calls of lexical `&`-variables**
  (the largest category of the terminal residue — `-> &op { op(…) }` [`END:op` in S03-operators/set_*.t] and
  `my &junction = ::("&any")` [autothreading.t] calling a Callable as a bareword; works correctly; future VM-migration candidate) /
  `__mutsu_*` internals / concurrent CAS [lever B] / no-match error generation [carrier].
  Pushed the only pure-builtin category `any`/`all`/`one`/`none` down into `builtins::build_junction` (the interpreter's
  `builtin_junction` also delegates to the same fn = deduplication), and bypassed `try_native_function`'s Instance-guard for junction
  ctors only. pin `t/native-junction-ctor.t`(24).
  **Lesson: measurement shows the §2 terminal is already mostly drained. The remainder is ③ state ownership (concurrency) / niche / the
  error carrier — the "high-traffic eradication" phase is over.**
  **Next: the main stronghold of ③ state ownership (the precursor of ④/⑤ folding env/types/handles into plain VM fields),
  pushing coercion (`List.Set/Bag/Mix`) down into builtins,
  or VM-nativizing the iterator protocol (`pull-one`/`skip-one`/`push-exactly`).**

- **`current_package` shared-handle migration (2026-06-12, registry-removal enabler)**: The registry's VM-native dispatch reads
  (`has_proto`/`has_multi_candidates`/`resolve_function_with_types`/…) are coupled to `current_package`
  (FQ-name construction via `format!("{pkg}::{name}")`), and as long as `current_package` was a plain `String` field on Interpreter,
  the VM could only read it via the `self.interpreter.current_package()` bounce, and could not serve these dispatch reads through the
  VM's own `registry` handle (the comment on `vm.rs::registry_mut` explicitly documented this blocker). This slice lifted `current_package`
  to an **`Arc<RwLock<String>>` shared handle** of the same shape as `registry`/`io_handles`/`output_sink`:
  - field `String` → `Arc<RwLock<String>>`. Access is only via `current_package()` (read-clone → owned `String`) /
    `set_current_package()` (write). The guard never escapes the accessor, so **no lock is held across re-entry**
    (the same discipline as the registry accessors = re-entry-safe, no deadlocks). The type change forced the compiler to enumerate
    all ~130 reads / ~27 writes.
  - `clone_for_thread` and construction of embedded regex/grammar sub-interpreters take a **snapshot** (fresh lock; thread-local
    registry semantics = child gets a copy, no write-back) and do not share the Arc.
  - Added the VM's own handle clone (`current_package_handle()`) + VM-side `current_package()`/`set_current_package()`.
    Moved the VM's ~55 sites from the `self.interpreter` bounce to the VM's own handle. Because the VM owns the Interpreter by value,
    both peers observe the same value through the same lock even during ping-pong = behavior-invariant. Save/restore semantics are also preserved.
  - Verification: cargo test 461/0, clippy/fmt green, package / `our sub` / inheritance / multi / `module our $x` / grammar token /
    `start{}` threading smoke all matching. (`t/native-array-mut.t` subtest 26 is PR-5's known type-meta Arc-ptr keying aliasing flake,
    unrelated to this change.)
  - **The payoff of registry removal (2026-06-12, #2929/#2933)**: Turned `Registry::has_proto`/`has_multi_candidates`/
    `has_declared_function`/`has_multi_function`(current_package, name) into pure `impl Registry` methods and moved VM dispatch to
    native reads via `self.registry`+`self.current_package()`. **Pure-predicate dispatch reads are exhausted.**

## Key design finding: env **cannot** use `current_package`'s shared-handle playbook (2026-06-12)

Investigation prompted by the user directive "start the ③ env migration" (2026-06-12) established that **env fundamentally cannot use
the `Arc<RwLock>` shared-handle approach of registry/current_package/io_handles/output_sink** (the hope that "current_package
migrated despite the doc's pessimistic prediction, so env might too…" is refuted):

1. **In-place mutation held across re-entry**: current_package could be lock-ified because it is read-clone-drop (the guard is dropped
   immediately and never spans re-entry).
   env, via `env_mut()`, **holds the mutation across re-entry** (re-entering user code while holding env mid-opcode).
   Holding a write guard on an `Arc<RwLock<Env>>` across re-entry = **self-deadlock**. Clone-per-access does not propagate mutations.
2. **Perf on the hottest path**: env carries all variable reads/writes (VM 483 sites + the entire runtime). Even the registry cost
   +2-4% with the transitional RwLock. A per-access lock on env would be a regression of a different magnitude (`Env` is already an
   internal `Arc<HashMap>` COW = an outer lock is meaningless).

→ **Reconfirmed: ③ env is not handle-migration-driven but "fallback-eradication-driven", exactly as stated at the top of this
document** (§1/§2 tree-walk fallbacks nativized → once the last reader is gone, env folds into a plain VM field = ④/⑤) — this is the only path.

### method-fallback landscape measured (2026-06-12, `MUTSU_VM_STATS`, method-heavy whitelist sample)

The dominant categories of method fallback remaining in the catch-all (the native/Buf/Failure fork of `vm_call_method_compiled.rs`):
`iterator`=280 (`.iterator` acquisition — largest), `new`=99 (③ ctor, env/BUILD-dependent), `push-exactly/at-least/all/until-lazy`=~200
(the iterator PUSH protocol = external-buffer writeback = first-class containers Phase 2), `can`=56 (MOP carrier), `raku`=40, `bool-only/count-only`,
`map`/`grep` (lazy/block form), `EVAL` (carrier), `Mix`, etc. **The largest env-independent tractable category = `.iterator` construction.**

### Slice: VM-nativizing `.iterator` construction (this PR)

The main path of `.iterator` (Range/Set/Bag/Mix/List/Array → an `Iterator` instance `{items, index:0, is_lazy?, known_count?}`) is
**pure value construction** (no env / no re-entry). Extracted this into the single implementation
`src/builtins/iterator_construct.rs::build_iterator_instance`, shared by the interpreter (the pure tail of
`dispatch_iterator_method`) and the VM (`try_native_iterator_construct`, just before the catch-all)
(**1 operation = 1 implementation**). `Seq` (consumed-state tracking + `squish`'s env mutation = interpreter-owned) and already-built
Iterator instances (identity return) fall through / branch. Measured: `iterator` fallbacks in range-iterator.t 720→120 (remaining 120 = Seq etc.).
Behavior-invariant (bag/mix/range/set-iterator, gather, S32-list/iterator roast green; the pre-existing 3 fails in S07-hyperrace/basics
are unrelated to this change).

## Investigation record: cross-closure visibility of dynamic variables depends on "instance mutation writeback" (2026-06-10)

While attempting the lexical `&`-var dispatch slice of Track A (PLAN.md reorganization Phase I), running a bound
Callable via `code()`/`&code()` on the VM (`vm_call_on_value`→`call_compiled_closure`) produced a regression where **writes to a
dynamic variable rebound by the caller (`$*ERR` etc.) do not propagate to the caller**. **This is a pre-existing bug reproducible
with `&code()` before my change** (with `sub cap(&code){ my $*ERR=FakeIO.new; &code() }`, `note` inside `code` does not reach the
rebound `$*ERR`).
The comment in `main`'s `exec_call_func_op` (~185-207) already documents this trap and deliberately leaves pure lexical `&`-vars
on the interpreter terminal.

### Full root cause (confirmed by measurement)

1. `note` → `write_to_named_handle("$*ERR")` → `get_dynamic_handle` → `self.interpreter.env().get("$*ERR")`.
   The **instance is resolved** via env (same instance id confirmed = not a different instance).
2. `$*ERR.print(...)` is a mutating method → `overwrite_instance_bindings_by_identity(class, id, updated)`
   (`methods_mut.rs:415`) writes back by updating "every env binding holding an instance of the same id".
3. **That writeback uses `self.env.values_mut()` = in a scoped env it walks only the overlay tier** (`src/env.rs`:
   "insert/remove/get_mut/iter touch only the overlay. parent is an immutable `Arc<Env>`"). The closure frame's overlay does not
   contain the caller's `$*ERR` binding (that lives in the parent tier), so the mutation **never reaches the caller's binding** and
   evaporates on frame return.
4. The interpreter (`call_sub_value`) runs the closure with `new_env = saved_env.clone()` (a **flat clone** of the caller env) and
   on exit merges side effects into "variables that already exist in the caller" (`resolution.rs` ~1317/~1564). Because it is flat,
   `values_mut()` can update the caller binding directly, and the merge propagates it. That is why the interpreter path works correctly.

### Conclusion: this is the ③ dual-store + value-identity (Phase 3) mountain, not a quick fix

"Just put dynamic vars in env" is insufficient (env is a scoped overlay with an immutable parent). The essence is that
**instances/containers are held by value, and mutation writeback only affects the current frame's overlay**. Not limited to `$*ERR`:
any "mutating method on an instance/array/hash held by a caller variable" inside a closure can break with the same structure
(map/for happen to work because they execute **in the same frame**, so overlay=caller by accident).

### Fix options (design targets for the next session)

- **(A) Make writeback walk the whole env chain**: Update `overwrite_*_bindings_by_identity` in the parent tier too, not just the
  overlay. But the parent is a shared, immutable `Arc<Env>`, and `Arc::make_mut` breaks O(1) scoped sharing and affects other
  holders too = conflicts with the scoped-env design. Needs redesign.
- **(B) Extend the closure exit-writeback to instance-by-id propagation**: The current exit writeback (`vm_closure_dispatch.rs`
  ~637-700) propagates only declared/captured locals. Extend it to "reflect same-id instances mutated in the overlay into the
  restored caller env" = mimicking the interpreter's merge. **The most local option**, but requires tracking mutated instances
  (currently untracked). Measurement shows that even when the overlay contains `$*ERR` (a captured clone when capture-skip is not
  yet applied), it did not propagate = it is not in the target set of the exit writeback.
- **(C) First-class instance/container cells (Phase 3)**: Reference-ify instances via shared cells (similar to `ContainerRef`),
  making mutation by-reference. Visible in all frames = the structural solution. The long-term correct approach but the largest rework.

### Correctness items confirmed along the way (should ship together with the fix)

- Dynamic vars (`*` twigil) are **not lexically captured** in Raku. The captured-env merge of `call_compiled_closure`
  (`vm_closure_dispatch.rs` ~230) puts a captured `$*ERR` (stale clone) into the overlay, shadowing the live parent.
  capture-skip (excluding twigil `*` via `is_dynamic_var_name`) is **necessary for read-side correctness**, but since the writeback
  gap (item 3 above) remains, it is **insufficient on its own** (capture-skip alone still fails to propagate to the caller).
  Must ship together with (A)/(B)/(C).
- Control-flow names (`return`/`take`/`emit`/`callsame`/… handled directly by `call_function`'s match) must always be excluded from
  lexical `&`-var dispatch (otherwise `&r=&return` rebinding causes infinite recursion. Several names are absent from
  `is_builtin_function`, so an explicit list is needed).

**Therefore Track A's lexical `&`-var dispatch is on hold until one of (A)/(B)/(C) lands** ([[project_lexical_amp_var_blocked]]).

## Phase II kickoff: env fold feasibility measured (2026-06-14, after Phase I close)

Following the user directive "start Phase II (env fold feasibility survey + first slice)" (2026-06-14), we established — by
**code measurement + `MUTSU_VM_STATS` per-name histogram** — which structures currently block the env fold. The conclusion
**refines** the design at the top of this document (③ = fallback-eradication-driven; env's end state is a plain field).

### Three measured facts

1. **The VM's direct env accesses = 481 sites** (`self.interpreter.env`/`env_mut`). If env became a VM field this would be a
   mechanical rename, but turning it into a `VM::env()` accessor **triggers borrow-checker conflicts** (the accessor borrows all of
   `&self`, whereas the current `self.interpreter.env()` is a partial borrow of `self.interpreter`), so at sites where an env read
   interleaves with other self fields it is not a pure sed. = Even seam-ification is non-trivial.
2. **What physically blocks the env fold is the ~15 remaining tree-walk delegation sites** (`call_sub_value`/`call_function[_fallback]`/
   `call_method_with_values`/`run_block_raw`). These interpreter tree-walk methods directly manipulate `self.env` during
   param-binding and then run the inner VM via ping-pong, so pulling env out to the VM breaks them.
3. **The dominant traffic of the remaining tree-walk is carrier/concurrency** (measured on the whitelist sample): method-fallback top =
   `Promise.at`=2000 / `await`=1200 (= **concurrency = Track C**), `WHAT` (MOP carrier), `EVAL` (carrier);
   the rest is niche (`map`/`CALL-ME`/`tree`/`parse` each ~10-20). **General user-code tree-walk is exhausted.**

### Core re-recognition: there is no "isolated first slice" for the env fold

- **The hot per-call-frame state (env / readonly_vars / let_saves) can only be folded all at once**: all of these are the hottest
  state, saved/restored on every function call. The `Arc<RwLock>` handle playbook (4 precedents: registry/io_handles/output_sink/
  current_package) **wrecks perf with per-access locks** (same reason as env) = no early migration. Plain-field-ification is only
  possible after tree-walk reader eradication. → **The idea of "moving readonly_vars/let_saves over first" is also impossible on
  perf grounds** (this **corrects** the doc's older wording "relatively local = early candidates": these are just as hot as env and
  cannot move until the final fold).
- **The carriers that read env are un-eradicable Raku semantics**: `type_matches_value` (lines 221-910, ~690 lines) evaluates a
  subset's `where` clause via **`eval_block_value`/`call_sub_value` + `self.env` reads/writes** (lines 514/523/533) = a carrier
  that runs user code. Likewise EVAL, regex-embedded `{}`, and `await`/Promise `.then` closures read env.
  These are not to be "removed"; instead, **when env eventually becomes VM-owned, a mechanism is needed to lend env to carrier
  execution (env-loan: the VM temporarily moves env in → interpreter executes → moves it back, or passes `&mut env`)**. = The final
  fold of Phase II is not "eradicate all tree-walk → pull env out" but "**make env VM-owned and hand it to carriers via env-loan**".

### The only cool state cleanly migratable early = `instance_type_metadata` (next slice candidate)

The type-metadata side table was embedded into all container values in Q2 (#2952–2985), but **the type metadata of Instance
values alone remains in the side table `Interpreter.instance_type_metadata: HashMap<u64, ContainerTypeInfo>`** (mod.rs:1037).
Access is **only 5 sites — insert(4716) / get(4787 = inside `container_type_metadata`) / clone(5589 = clone_for_thread)**
= no borrow held across re-entry, cool (write at instance construction / read at type check) = **the handle playbook applies most
cleanly here**. Making this a VM-readable handle would allow **VM-nativizing the non-subset path of `type_matches_value`
(simple type names decided by registry + value_type + instance_meta; subsets are rare)**, removing most of the 41 bounces
(only subsets keep the carrier fallback).

#### DONE: handle-ified the cool side table `instance_type_metadata` + made `container_type_metadata` a VM-native read (CP-3 Track 1)

- **PR-1 (#3068, existing)**: Shaped `instance_type_metadata` into `Arc<RwLock<HashMap<u64, ContainerTypeInfo>>>`
  (the shared-handle playbook of `current_package`/`io_handles`). `clone_for_thread` takes an explicit snapshot (map deep-copy).
- **PR-2 (this slice)**: VM-nativized `container_type_metadata` (**the type-metadata read for Instance values**).
  - Extracted the READ logic into the module-level free function **`container_type_metadata_with(value, instance_meta)`**, so that
    `Interpreter::container_type_metadata` and the VM's peer-handle read **share a single implementation** (1 operation = 1 implementation).
    Container values (Array/Hash/Set/Bag/Mix) read the embedded metadata; only Instance values look up the shared map by id.
  - Added to the VM the peer field `instance_type_metadata` (cloned at `VM::new` via `instance_type_metadata_handle()` —
    a stable handle of the same shape as registry/io_handles) + `VM::container_type_metadata(&self, value)`.
  - Replaced the VM's **~29 sites** of `loan_env!(self, container_type_metadata(...))` with `self.container_type_metadata(...)`.
    **This read never touches env**, so neither env-loan nor the interpreter bounce was ever needed (the loan was pure
    unnecessary overhead/coupling). Writes (`register_container_type_metadata`) stay via the interpreter = visible to VM-native
    reads through the shared lock. Behavior-invariant (typed array/hash/native-int/num, subset, declare whitelist green, make test PASS).
  - **Next cool side-table candidates**: `var_type_constraint`/`var_default`/`var_hash_key_constraint` (type/default side tables —
    the env-bound type-name part remains an open issue) / `state_vars`/`shared_vars` (dedicated HashMaps — the env sync path remains an open issue).

### The realistic way forward for Phase II (established by measurement. PLAN.md "Critical path" is canonical)

- **critical path (the substance) = the env-loan mechanism for carriers**: make env VM-owned and let EVAL/subset-where/regex-`{}`/
  Promise-`.then` borrow the VM-owned env for execution. Only once this lands can env be pulled out of Interpreter (→ dual-store
  removal → Interpreter removal). The hot state env/readonly/let goes **all at once** in this final fold (early splitting is
  impossible on perf grounds).
- **off-critical-path (optional, parallel) = handle migration of cool side tables**: handle-ifying `instance_type_metadata` etc.
  unlocks VM-native dispatch (the type_matches non-subset fast path, etc.) (same shape as the precedent where current_package
  unlocked registry pure-predicate dispatch), but **this does not directly advance the env fold = not on the critical path**
  (it is perf/cleanliness-oriented bounce-count reduction). As a shrinking of the final fold surface, bundle it into CP-3 or
  pursue it optionally.
- **The concurrency tree-walk (await/Promise) runs in parallel as Track C** (the largest remaining traffic, independent of the env fold).

---

## CP-3 collapse PoC: eliminating the ping-pong (`VM::run_nested`, 2026-06-15)

> Reconfirming the premise (measured): **a tree-walk execution engine no longer exists** (no `eval_expr`/`eval_stmt`; all execution is
> compile→bytecode→VM). ∴ "deleting the tree-walk interpreter" = the structural work of dissolving the `Interpreter` struct into the
> VM, and the only genuine design problem is the **ping-pong** (the bidirectional ownership where `Interpreter`-side carriers spin up
> a sub-VM each time via `mem::take(self)`+`VM::new(self)`+`*self=interp`). Whether this can be replaced by "direct re-entrant
> execution on the VM" is the linchpin of completability.

**PoC = `VM::run_nested(&mut self, code, fns)`** (`src/vm.rs`). Re-entrantly executes a compiled block on the existing VM,
without spinning up a sub-VM:
- The **execution registers** that `VM::new` freshly initializes (stack/locals/call_frames/resume_ip/topic family/each context flag) are
  saved → reset fresh → `run_inner` → restored. **Shared state (interpreter fields, env, registry/io/output handles) is NOT reset**
  (the nested block observes and mutates the same state = the same semantics as the ping-pong's inner VM sharing the moved
  interpreter and the loaned env). Caches are generation-managed, so keeping them across the call is correct.
- **Wiring**: `vm_run_block_raw` (execution of deferred role-body statements) went from `loan_env_for(run_block_raw)` [ping-pong] →
  `interpreter.compile_block_raw` (pure compilation, no env needed) + `self.run_nested(...)` + the DESTROY pass.
  The compile part of `run_block_raw` was extracted as `compile_block_raw` and shared.

**An important dual-store interaction the PoC surfaced (guaranteed to matter throughout the campaign)**: the ping-pong performed the
re-synchronization of env mutations to the outer scope **implicitly** (the inner VM's env came back via the interpreter and the caller
re-synced). With in-place re-entry that no longer happens, producing a regression where **nested execution mutates an outer lexical
(`my $x`) via env → the restored outer locals are stale** (mutations of `$side` vanished across multiple deferred statements).
Fix = set `env_dirty = true` at the end of `run_nested` so the outer execution re-syncs from env before its next local read.
**Lesson: when removing the ping-pong, each site must make "reflecting env mutations into locals" explicit (the dual-store is permanent).**

**Conclusion: the linchpin is solved.** `make test` PASS, role roast green, A/B behavior-invariant (after the env_dirty fix). pin =
`t/run-nested-role-body.t`. Next: convert the other carriers (`eval_block_value`/`call_sub_value`/subset-where eval/…) to
`run_nested` one by one, and finally remove the `Interpreter`-side carriers and the ping-pong/loan → struct dissolve.

---

## CP-3 final collapse — execution plan for the next session (finalized 2026-06-15)

> ### ✅ DONE (PR #3102, 2026-06-15; CI full `make roast`+`make test` PASS, net −794 lines)
>
> The big-bang collapse was executed end-to-end and landed. **`struct VM` has been dissolved into `Interpreter`** (a single struct
> that IS the bytecode VM).
> - **The direction is the reverse of the doc's original plan**: survivor = **`Interpreter`** (it is the public entry type, so
>   no API/entry changes). The `pub(crate) type VM = Interpreter` alias left the ~40 `impl VM` blocks untouched (the cosmetic rename
>   is follow-up cleanup). Executed step 1 (field merge — the 718-site rewrite was largely compressed by the alias) /
>   step 2 (ping-pong→`run_nested`/`with_nested_registers`, carrier relocation) / step 3 (thinning the loan machinery into plain
>   self-calls, deleting the dead machinery).
> - **Hit and fixed exactly the trap the run_nested PoC predicted**: the catch_unwind panic boundary that the old ping-pong `VM::run`
>   established was absent from `run_nested`, so a Rust panic in a `dies-ok`/`try` block via `run_compiled_block` bypassed try/CATCH
>   and aborted (surfaced by `t/vm-panic-boundary.t`) → extracted the boundary into `run_inner_guarded` and wired it into both
>   run_top and run_nested. env-resync is guaranteed by `run_nested`'s `env_dirty=true` (the run_reuse loop is likewise guaranteed by
>   `with_nested_registers`).
> - **Remaining cleanup (independent, low-risk, follow-up PRs)**: ① the cosmetic rename removing the `VM` alias, ② complete removal
>   of `Env::poisoned` (field+debug_assert).
> - The `env_dirty` dual-store persists, per CP-2.
>
> The pre-start plan follows (kept for the record).

> ### incremental is exhausted. What remains is "the final collapse itself" = big-bang (established by measurement)
>
> The 2026-06-15 session established, via `MUTSU_VM_STATS` measurement + close code reading, that **the pickable incremental slices
> have run out**. This fact is the premise for the next session:
> - **dispatch fallback = drained** (what remains: `new`=ctor [③-blocked] / `isa`/`does`=already VM-native / MOP / EVAL=carrier.
>   Single-digit, diffuse; no hot category left to win by nativization).
> - **The MOP methods (does/isa) are already VM-native** (`vm_arith_ops`/`vm_call_method_ops`).
> - **The hot state fields (env/readonly_vars/let_saves/var_defaults/state_vars…) are per-call interpreter dispatch state**.
>   `save_readonly_vars`/`restore_readonly_vars` are saved/restored on every function call across the dispatch/param-binding paths of
>   **9 files**, and `&mut`-returning accessors (`readonly_vars_mut`/`var_type_constraint_fast`) are also involved. **No early fold.**
> - **The cycle**: the hot fields are saved/restored by the interpreter dispatch carriers → the carriers must go before the fields can
>   go → but the carriers (call_function/call_sub_value param-binding) are already mostly handled natively by the VM
>   (`call_compiled_closure`) with the interpreter versions as fallback → removing the fallback = removing the struct.
>   **∴ They cannot be folded incrementally one by one; the collapse is all-at-once.**
>
> → **The remaining work = the big-bang collapse of the Interpreter struct itself.** It cannot be reached by stacking individual slices.

### Current state (main; the scaffolding for the collapse is in place)

- **env**: VM-owned (CP-1 #3075; lent to carriers via `loan_env!`/`loan_env_for`).
- **Shared handles (6)**: registry / io_handles / output_sink / current_package / instance_type_metadata (+ env).
- **`VM::run_nested` (#3095)**: the mechanism for re-entrantly executing a compiled block on the VM without the ping-pong.
  **The core tool of the collapse.** Saves/resets/restores the execution registers + `env_dirty=true` for env→locals re-sync.
- **Carriers already on run_nested**: `vm_run_block_raw`(#3095) / the pure-expr part of `vm_eval_block_value`(#3096).
- **The dual-store (`env_dirty`/`saved_env_dirty`/`ensure_locals_synced`/`sync_locals_from_env`) is permanent** (decided in CP-2;
  it stays inside the VM after the collapse).

### Collapse procedure (long-lived branch; each substep verified by `make test` + full roast = CI)

> Principle: physically converge on one struct while staying behavior-invariant. The borrow checker and CI are the safety net.
> **Do not add incremental perf/carrier slices** (ROI is zero before completion).

1. **Struct merge (mechanical; the largest volume)**: physically move the remaining ~67 fields of `Interpreter` into the `VM` struct
   (env + the 6 handles are already migrated). Bulk-rewrite the VM's **718 sites** of `self.interpreter.X` → `self.X`
   (purely mechanical = multi-agent fan-out is possible here). Preserve the snapshot semantics of `clone_for_thread`.
2. **Carrier/dispatch method relocation (~192)**: move the methods of `impl Interpreter` to `impl VM` (or free functions). The
   ping-pong (`mem::take(self)`+`VM::new(self)`+`*self=interp`) is replaced by **direct re-entry via `run_nested`/`run_reuse`**.
   - **★ The most important trap (proven by the run_nested PoC)**: the ping-pong performed env-mutation → outer-locals re-sync
     **implicitly**. Direct re-entry does not, so **each former ping-pong site must make the env→locals reflection explicit
     (`env_dirty=true` etc.)** — otherwise stale-locals bugs (of the vanishing-`$side`-mutation kind). This cannot be mechanized;
     manual verification is mandatory (A/B + roast).
   - Order: shallowest dependencies first. EVAL is VM-nativized via compile→`run_nested` (no separate struct needed; nothing to keep).
     subset-where eval also goes to `run_nested` (base-check/coerce stays a non-executing bounce from the VM to interpreter helpers).
3. **Removal of the loan/ping-pong machinery**: delete `loan_env!`/`loan_env_for`/the poison guard (`MUTSU_POISON_DIAG`)/the pull in
   `VM::new(interp)` and the push-back in `run`/`into_interpreter`.
4. **Delete the `Interpreter` struct**: once it holds zero state and zero carriers, remove the `self.interpreter` field and the struct
   in `runtime/`. Native builtins / static-analysis helpers / registration remain in `runtime/` (or `builtins/`) as free functions or `impl VM`.
5. **The dual-store stays**: `env_dirty`/`locals↔env` sync persists as a VM-internal optimization (CP-2).

### Risks and mitigations

- **Major borrow-checker upheaval** (the struct merge) → incremental compilation on a long-lived branch. Whole-`self` borrow vs
  partial borrow conflicts are resolved individually by extracting local bindings (the technique proven in CP-1 1b/1c).
- **Missed env-resyncs** (the run_nested lesson) → explicit at each carrier + A/B + full roast. Verify both behavior and perf on the
  debug build (MUTSU_VM_STATS is opt-level-independent).
- **Concurrency (`clone_for_thread`)**: preserve the snapshot semantics (map deep-copy).
- **Perf regression** (touching the hottest env/dispatch) → the release timeouts of `make roast` are the safety net (lesson of #2746:
  perf regressions are undetectable by make test and surface in CI's release roast). Verify heavyweights like fib/int.t timed.

### resourcing

- **Manual work is the mainline** (struct merge design, ping-pong removal, env-resync each have individual traps).
- **Multi-agent fan-out is deployed only for the purely mechanical site rewrite of step 1 (718 sites `self.interpreter.X`→`self.X`).**
- Completion criteria = `Interpreter` struct deleted; VM is the sole execution engine; all state VM-owned; only the `env_dirty`
  dual-store remains.

### Pre-start checklist

- [ ] Create a long-lived branch from main (`cp3-collapse`).
- [ ] Step 1 (struct merge + 718-site rewrite) in one PR (huge but mechanical, behavior-invariant). Confirm CI green.
- [ ] Split step 2 into PRs per carrier group; each with explicit env-resync + A/B + roast.
- [ ] Steps 3/4 together at the end.
- [ ] In each PR, run `make test` + relevant roast locally; delegate the full roast to CI.

---

## CP-1 env-loan design (finalized 2026-06-15; [PLAN.md](../PLAN.md) CP-1 step 1a)

> This section is the deliverable of CP-1 step 1a (design finalization; doc only). It finalizes the mechanism (**env-loan**) by which
> env moves to single VM ownership and tree-walk carriers borrow env only for the duration of execution. Based on close reading of the
> actual code (the ping-pong machinery, the Env structure, the env read/write timing of every carrier). The subsequent 1b–1e
> implementation follows this design.

### Current model (measured 2026-06-15)

- **env's address**: `Interpreter.env: Env` (`src/runtime/mod.rs:837`, private field). Accessors:
  `env()` (read, `mod.rs:4345`) / `env_mut()` (write, `accessors.rs:616`) / `clone_env()` (= `env.flattened()`) /
  `set_env`/`take_env`.
- **The VM's env access**: the VM **owns `interpreter: Interpreter` by value** (`vm.rs:132`). The VM tree reads env via
  `self.interpreter.env()` (**264 sites** read) / `env_mut()` (**249 sites** write) = **513 sites total**.
  Per-module skew: `vm_var_assign_ops`=98, `vm_control_ops`=86, `vm_misc_ops`/`vm_call_dispatch`=36 each, …
- **`Env` is a small COW structure**: `Arc<HashMap>` overlay + `Option<Arc<Env>> parent` + `Option<HashSet> tombstones`
  + `u16 depth` (`env.rs:88`). clone is an O(1) Arc bump; the scoped overlay is a per-frame transient (flattened via `flattened()`
  at capture time). **`mem::swap`/`mem::take` are a few-word memcpy with no alloc and no refcount change** (the reason env-loan
  is cheap).
- **Re-entry (VM ⇄ tree-walk) takes 2 forms**:
  1. **ping-pong (fresh VM)**: `let interp = mem::take(self);` (= `&mut Interpreter`) → `VM::new(interp)` →
     `vm.run(code, fns)` → `(interp, result)` → `*self = interp` (`run.rs:960-963` run_block_raw,
     `resolution.rs:1930-1942` run_compiled_block, …). **The whole Interpreter (env included) round-trips by value between nested VMs.**
  2. **run_reuse (persistent VM)**: set up `VM::new(interp)` once (`resolution.rs:2092` eval_map_over_items,
     2331, 2521), and repeatedly execute the loop body via `vm.run_reuse(&mut self, …)` (`vm.rs:799`). Between iterations the caller
     writes env via `vm.interpreter_mut().env_insert(...)` (2116-2133), reads via `vm.interpreter().env().get("_")` (2141), and
     finally reclaims the Interpreter via `vm.into_interpreter()` (2097).
- **`VM::new(interp)`** only clones the registry/io_handles/output_sink/current_package handles (`vm.rs:482`).
  env still stays in `interp.env` while the VM owns it by value. **`VM::run` returns `(self.interpreter, result)`** (`vm.rs:695`).

### The env borrow points of the carriers (full enumeration: read/write/re-entry timing)

"carrier" = a tree-walk execution path the VM delegates to, which reads/writes `self.env` during execution. They split into two
kinds: **live** (reads/writes the live env and reflects it back to the caller) and **snapshot** (clones env, runs on it, no
write-back). This is the core fork of the loan mechanism.

| carrier | definition | env READ | env WRITE | re-entry | kind |
|---|---|---|---|---|---|
| **EVAL** | `builtin_eval`→`eval_eval_string`(`system.rs:1221`) | takes a snapshot; reads `$_`/`=pod`/`__mutsu_in_eval`(1235-1244) | inserts/restores `__mutsu_in_eval` etc.; **the inner code sees and mutates the enclosing scope's env**(1254,1317-1336) | `parse_and_eval_with_operators`→`run_block_raw`(ping-pong) | **live** |
| **subset `where`** | `type_matches_value`(`types/type_matching.rs:221`) | bound pkg(487), `$_`(533) | sets/restores `$_`(534,540-542) | `eval_block_value`/`call_sub_value`(ping-pong) | **live** |
| **regex embedded expr** | `eval_string_as_source`(`regex_parse.rs:6061`) | passes `self.env.clone()` to a **fresh Interpreter**(6068) | none (snapshot) | new Interpreter→`eval_block_value` | **snapshot** |
| **Promise/start/thread** | `clone_for_thread`(`mod.rs:5377`) | flattens env, seeds shared vars, child gets `env: self.env.clone()`(120) | none (snapshot, no write-back) | child fresh Interpreter→VM | **snapshot** |
| **call_sub_value** | `resolution.rs:1096` | env reads/writes during param binding | param binding, scope save/restore | `run_compiled_block`(ping-pong) | **live** |
| **call_function[_fallback]** | `builtins.rs:315`/`builtins_operators.rs:7` | env for arg resolution/binding | param binding | inner VM | **live** |
| **call_method_with_values** | `methods.rs:310` | env | method body env | inner VM | **live** |
| **run_instance_method** | `class.rs:725` | env | method body env | inner VM | **live** |
| **eval_block_value** | `resolution.rs:1712` | captures `&`-vars + callable ids(1721-1726); `env.clone()` for trailing sub(1764) | runs the block on `self.env` | `run_compiled_block`(ping-pong) | **live** |
| **run_block_raw** | `run.rs:940` | — | runs the block on `self.env` | `mem::take`+`VM::new`(ping-pong) | **live** |
| **map/sort reuse** | `eval_map_over_items`(`resolution.rs:2092`) | `vm.interpreter().env()`(2141) | `vm.interpreter_mut().env_insert` per-iter(2116-2133) | `VM::new` once + `run_reuse` loop | **live (externally driven)** |

VM→carrier delegation site counts (grep within vm/, 2026-06-15): `call_function`×7, `call_function_fallback`×3,
`call_sub_value`×5, `call_method_with_values`×1, `run_block_raw`×1, `run_instance_method`×1,
`type_matches_value`×42. type_matches is mostly pure type-name checks (no env needed); only the subset `where` path touches env.

### Mechanism decision: **adopt approach A (lending via move/swap). Approach B is rejected**

- **Approach A (adopted) = env normally lives in `VM.env` and is swap-lent to the interpreter around carrier calls**:
  - `VM::new(interp)` **pulls** `interp.env` into `VM.env` (`interp.env` is emptied via `mem::take`).
  - `VM::run`/`into_interpreter` **push back** `VM.env` into `interp.env` before returning the Interpreter
    (so the reclaimed Interpreter has a coherent env for carriers / the next ping-pong).
  - Carriers keep reading `self.env` as Interpreter methods. **Right before the VM calls a carrier, it does
    `mem::swap(&mut self.env, &mut self.interpreter.env)`, swapping back on return**. The ping-pong inside a carrier
    (`run_compiled_block`/`run_block_raw`) takes the Interpreter with env included via `mem::take(interp)`, the inner
    `VM::new` pulls env into the inner `VM.env` → runs → pushes back → `*self = interp` → the carrier sees the updated env
    → the VM reclaims it via swap-back. **Fully coherent.**
  - **Snapshot carriers** (thread/regex interpolation) need no swap — just hand them a `&` to `self.env` (= `VM.env`) and let them
    `clone()` (no write-back). The loan applies to live carriers only.
  - **Why A**: the ping-pong already round-trips the Interpreter by value, so adding an env pull/push to `VM::new`/`run` is a
    **local seam change**. The carrier population (all of runtime/ is written assuming it reads `self.env`) is **untouched**.
    swap is a few-word memcpy (see above), with no impact on the hot path.
- **Approach B (rejected) = pass `&mut Env` to carriers as a method argument**: env is read as `self.env` across effectively the whole
  runtime/ tree-walk (`type_matches_value` 26 files, `current_package` 33 files, env is the entire runtime).
  Threading `&mut Env` through every carrier and every function it transitively calls is unrealistic (same root cause as why
  "field relocation first" at the top of this doc is impossible). **Rejected.**

### Seam strategy (1b–1e; each PR behavior-invariant; CI = make test + full roast is the safety net)

By inserting an **accessor seam** before physically moving env, the migration of the 513 sites splits into "mechanical,
behavior-invariant" and "the dangerous flip":

- **1b (seam introduction, 3-4 PRs)**: introduce `VM::env()`/`VM::env_mut()`, which **for now forward to
  `self.interpreter.env()`/`env_mut()`** (= completely behavior-invariant). Migrate the 513 sites to the accessors (starting from
  module groups without borrow conflicts: var_assign → control → call/dispatch → misc/helpers → the rest). **Externally driven sites
  are also in scope**: the `vm.interpreter().env()` / `vm.interpreter_mut().env_insert()` in `resolution.rs` (map/sort reuse) are
  also aligned to use `vm.env()`/`vm.env_mut()` (once env moves to the VM in 1e, env disappears from `vm.interpreter()`, so putting
  these on the seam is mandatory).
- **1c (borrow-conflict resolution, 1-2 PRs)**: the accessors are **whole borrows** of `&self`/`&mut self`. Sites where an env read
  interleaves with other self fields (currently passing via the partial borrow of `self.interpreter.env`) are resolved individually
  by extracting local bindings / splitting scopes. On completion, the VM-side env access is 100% via the seam.
- **1d (organizing the carrier borrow points, 1-2 PRs)**: reorganize the places where live carriers touch `self.env` into a shape
  where env can be swapped in/out at method boundaries (behavior-invariant). Confirm snapshot carriers use only `clone` borrows.
- **1e (physical migration + loan plumbing, 1-2 PRs; the biggest mountain)**: delete `Interpreter.env` and introduce `VM.env`.
  Flip the accessor bodies from `self.interpreter.env` → `self.env`. Implement `VM::new`=pull / `VM::run`+`into_interpreter`=push back.
  Wrap the live-carrier calls (the ping-pong delegation sites in the table above) in `mem::swap` lending. `make test` + local roast →
  push → **delegate the full roast to CI**.

### Invariants (implementation-time checklist)

1. **At every moment env exists "live" in exactly one place**: `VM.env` during VM execution, `interpreter.env` during carrier
   execution (while on loan). Never create a state where both hold a live copy (swap is a move, not a copy).
2. **Consistency with nested ping-pong**: the inner `VM::new`'s pull / `run`'s push back must not corrupt the outer loan state
   (the inner side moves the interp wholesale, so it round-trips correctly with the loaned env inside).
3. **Snapshot carriers do not take a loan**: thread/regex interpolation is a read borrow of `self.env.clone()` only. Do not create a
   swap-back path (writing back would break thread semantics = the independent copy).
4. **The run_reuse path**: env is already pulled by `VM::new`, so inter-iteration env writes go to `vm.env_mut()` (= `self.env`).
   `into_interpreter` pushes back before returning the Interpreter.

### ⚠️ Fatal facts discovered in the 1e implementation attempt (2026-06-15; **measurements that overturn the design's premise**)

On top of 1b (the seam), we **actually implemented the 1e flip** (adding a `VM.env` field + flipping the accessors to `&self.env` +
`VM::new`=pull / `run`/`into_interpreter`=push back + the `loan_env_for` swap wrapper for carriers [`type_matches_value` /
`vm_call_function` / `vm_call_sub_value` / `vm_call_function_fallback` / `vm_call_method_with_values` /
`vm_run_instance_method` / `vm_run_block_raw`]). **The build passed (zero borrow conflicts), but smoke broke widely**:
`@a.map(*+1)`→`(Any)`, `my Even $e = 4`→`(Any)`, `$*dyn` reads→empty.

**Root cause (confirmed by measurement)**: the interpreter methods called by the VM number **227 (distinct)**, of which
**62 read `self.env`** (scanning the first 50 lines of each method; more when counting transitive reads).
= The env-reading paths are **not at all** the "~15 delegation sites" of the earlier table, but **widespread**:
`var_type_constraint` (VM 40 sites) /
`get_dynamic_var` / `restore_let_saves` (transitively via `restore_let_value`) / `container_type_metadata` /
`var_default` / `var_hash_key_constraint` / `get_caller_var` / `push/pop_caller_env` / `proxy_fetch` /
`get/set_shared_var` / `sync_shared_vars_to_env` / `get/set_state_var` / `our_vars_iter` / `set_our_var` /
`restore_var_bindings` / `resolve_indirect_type_name` / `render_*_value` (user gist calls = carrier) …

→ **Physically moving env makes these 62+ methods read the "loaned-out, now-empty interpreter.env".** Lending via swap to carriers
alone is not enough; **all 62+ methods (hundreds of sites) would need to be wrapped in swap wrappers**.

#### Why "wrap all 62 methods" is also a bad idea

1. **A miss = a silent stale-env bug**: forget to wrap even one method that transitively reads env and it silently misbehaves reading
   an empty env (compilation still succeeds). Completely enumerating the transitive env dependencies of 227 methods is brittle.
2. **swap overhead on the hottest paths**: `var_type_constraint` (on every typed assignment / signature binding),
   `container_type_metadata`, etc. are the hottest. A `mem::swap` of `Env` (≈40B) is cheaper than a lock but not zero, and putting it
   per-call on the hottest paths can regress (same root cause as why env cannot be `Arc<RwLock>`-ified, at the top of this doc).
3. **The double-swap trap**: the wrappers sit only at the VM→interpreter boundary. Interpreter-internal mutual calls read self.env
   (on loan) directly, so no double swap occurs — but when "some env-reading method is called both from the VM and from inside the
   interpreter", only the VM path must be wrapped, requiring a triage of the dual-use methods.

#### Conclusion: 1e is not "one loan-plumbing PR" — **reducing the env-reading interpreter helper surface is a prerequisite**

The old PLAN 1e ("physically migrate env + lend to ~15 carriers") was **refuted by measurement**. For env to become solely VM-owned,
first the **62+ env-reading interpreter helpers called from the VM must be VM-nativized / folded into forms that receive env as an
argument** (= the CP-3-style surface reduction). Only once that progresses and "the VM→interpreter env-reading paths" shrink to just
the carriers (EVAL/subset-where/regex-`{}`/Promise.then) can the physical move happen via env-loan (swap lending) to the carriers.

= **The correct order is "1b (seam, done) → env-reading helper surface reduction (CP-3 pulled forward) → 1e flip + carrier loan".**
Forcing the flip first would be whack-a-mole across 62+ sites, with the double risk of silent bugs and perf regression. The trial
branch `cp1-1e-env-loan-flip` (broken; for reference) preserves the machinery (`loan_env_for` + pull/push + carrier wrappers) and can
be reused after the surface reduction.

#### What to do next (first slice candidates for the surface reduction)

VM-nativizing — and thereby making env-independent — the env-reading helpers **that can be replaced with state-family side tables**
(the same shape as the #3068 precedent that handle-ified `instance_type_metadata`) is the safe entry point:
- `var_type_constraint` / `var_default` / `var_hash_key_constraint` / `set_var_type_constraint` — the type/default side tables.
  If the part read from env (env-bound type names) can be decided via `registry` + value-type + the `instance_type_metadata` handle,
  they become env-independent.
- `get/set_state_var` / `get/set_shared_var` / `sync_shared_vars_to_env` — state/shared are dedicated HashMaps. Cutting the env sync
  path makes them env-independent.
- `get/set_our_var` / `our_vars_iter` — our vars are the package stash.
Each helper that stops reading env moves 1e closer to a carrier-only loan. Each slice is behavior-invariant (CI safety net).
