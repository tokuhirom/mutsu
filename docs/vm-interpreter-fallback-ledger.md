# VM → Interpreter fallback ledger

**Progress ledger** for the final goal "remove the tree-walking Interpreter execution path → delete the dual store"
([PLAN.md](../PLAN.md) ①–⑤). It enumerates, one by one, the sites where the VM (`src/vm/`) still delegates
to the Interpreter (`src/runtime/`), and deletes a row each time one is eliminated. Related:
[vm-interpreter-dedup.md](vm-interpreter-dedup.md) (duplicate removal),
[vm-single-store.md](vm-single-store.md) (the **current design** for unifying locals↔env = Slice F),
[vm-dual-store.md](vm-dual-store.md) (history of the withdrawn attempts at the same), [vm-decoupling.md](vm-decoupling.md) (dispatch).
Individual fallback eliminations are now understood as preliminary steps toward Slice F (single-store convergence).

## Terminology

- **True fallback** = something that should be executed as bytecode but is currently delegated to the tree-walk Interpreter.
  Marked in code with `// TODO: compile to bytecode`. Eliminating these is ①.
- **CARRIER** = essential delegation that is not an elimination target (reflection / MOP / EVAL sub-VM / metaprogramming hooks / mode state).
  It is a reference through which the Interpreter owns shared registries and execution state, not a tree-walk
  (to be "separated or made explicit" in ④). Marked in code with `// CARRIER:`. Once env/registry ownership moves to the VM in ③, these become mere shared references.

Current state of the visualization: `grep -rn "TODO: compile to bytecode" src/vm/` = 18 markers, `grep -rn "// CARRIER:" src/vm/` = 8 markers.
(Some markers bundle multiple adjacent sites. The tables below are canonical.)

## §1 — True fallbacks in method dispatch (`call_method_with_values` / `_mut_`)

| file:line | receiver / method | difficulty | blocker / dependency |
|---|---|---|---|
| `vm_call_method_compiled.rs` native-method (non-mut) | built-in class methods such as IO::Pipe/IO::Handle | HARD | ③ (the implementations `native_io_*` require interpreter-owned state such as file handles. The initial "can be done individually" assessment was wrong) |
| `vm_call_method_compiled.rs` catch-all (non-mut) | **native/Buf/Failure/MOP only** (user methods are all bytecode-compiled already) | HARD | ③ state ownership transfer. **Corrected by measurement in ③ PR-3**: this was **not** a user-method tree-walk after all |
| `vm_call_method_compiled.rs` native-method (mut) | same as above, mut | HARD | ③ (same as above) |
| `vm_call_method_compiled.rs` catch-all (mut) | same as above, mut (native/Buf/Failure/MOP only) | HARD | ③ |
| `vm_call_method_mut_ops.rs` catch-all | generic mut methods (excluding plain untyped `@`-array mutators and mutable Buf write methods) | HARD | ③ |
| ~~`vm_call_method_mut_ops.rs` plain `@`-array mutators~~ | ~~append/prepend/unshift/pop/shift~~ | — | **✅ Eliminated (③ PR-5)**: VM-native via `try_native_array_mut`. typed/shaped/lazy/shared/constrained stay on the interpreter |
| ~~`vm_call_method_mut_ops.rs` mutable Buf write methods~~ | ~~write-bits/write-ubits/write-num*/write-int*/write-uint*~~ | — | **✅ Eliminated (③ PR-7)**: VM-native via `try_native_buf_mut`. Pure conversions unified into `builtins/{buf_bits,buf_write_num,buf_write_int}`. type-object/Blob/malformed-arity stay on the interpreter |
| ~~`vm_call_method_mut_ops.rs` simple array-backed Iterator~~ | ~~pull-one/skip-one/skip-at-least/skip-at-least-pull-one/sink-all~~ | — | **✅ Eliminated (③ PR-9)**: VM-native via `try_native_iterator` (`$it.pull-one` is the CallMethodMut = mut path). Only self-contained `items`+`index` iterators. squish (callback) / lazy (gather/coroutine, `is_lazy`) / push-* (external buffer) / count-only/bool-only stay on the interpreter |
| ~~`vm_call_method_mut_ops.rs` array-backed instance~~ | ~~push/pop/shift on `is Array` storage~~ | — | **✅ Eliminated (#3058)**: `native_array_storage_mut` makes push/pop/shift/unshift/append/prepend against the backing storage of an `is Array`-backed instance VM-native. Instance rebuilt via `write_back_array_storage_instance`. Richer methods (join/sort/map/splice/AT-POS etc.) stay on the interpreter |
| `vm_data_ops.rs` shared push | `@a.push` (threaded) | HARD | lever B (shared cell ownership) |
| `vm_data_ops.rs` shaped push | shaped array push | MEDIUM | VM-ify shaped dimension metadata checks |
| `vm_data_ops.rs` non-simple push | ArrayPush targets that are neither Array nor ContainerRef (cold) | LOW | **Confirmed cold by probe (2026-06-14)**: the ArrayPush opcode is only emitted for single-argument push on a *local* array. Closure-captured / multi-arg push flows to CallMethodMut and **was made native in #3060** (below). The remaining ArrayPush non-Array branch has zero firing examples across the whitelist = cold |
| ~~`vm_call_method_mut_ops.rs` CallMethodMut push~~ | ~~closure-captured / multi-arg `@a.push`~~ | — | **✅ Eliminated (#3060)**: added a `push` arm to `try_native_array_mut`. `@a.push(x)` uses the ArrayPush opcode only for single-argument local; everything else (captured / multi-arg) arrives via CallMethodMut and is now VM-native. typed/shaped/lazy/shared/constrained stay on the interpreter |
| ~~`vm_smart_match.rs` key-method~~ | ~~key-method extraction for smartmatch~~ | — | **✅ Eliminated (PR2)**: moved to unified compiled-first |
| ~~`vm_call_method_compiled.rs` QuantHash coercion~~ | ~~`.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`/`.MixHash` (list-like receivers)~~ | — | **✅ Eliminated (③ PR-8 + MixHash slice)**: VM-native via `try_native_quanthash_coerce`. Pure folding unified into `builtins/quanthash_coerce`. **`.MixHash` also added** (the old exclusion reason "type metadata registration = interpreter-owned state" became stale with #2952's embedding of container values = metadata embedded in `Value::Mix`'s Arc makes it a pure value op; new `to_mixhash`). Only Instance/Package receivers stay on the interpreter |
| `vm_call_helpers.rs` hyper temp | hyper method on a temp-bound item | MEDIUM | first-class containers Phase 2 |
| ~~`vm_register_ops.rs` react loop~~ | ~~`run_react_event_loop[_drain]` / `run_whenever_with_value`~~ | — | **✅ Eliminated (Stage 1+2, #3010/#3027/#3029/#3031/#3038/#3039)**: unified the **4-way duplication** of the drive loop (react/await-promise/tap×2) into a single engine (tap×2→1 #3010; react↔await-promise into `SupplyDrivePolicy { React, Promise }` + `drive_react_subscriptions` #3027). **Stage 2 = loop ownership inversion complete**: moved `run_react_event_loop`/`drive_react_subscriptions`/`run_react_consumer`/`replay_static_supply` from `impl Interpreter` → `impl VM` (new `vm/vm_react_loop.rs`) #3038; switched whenever body/LAST/CLOSE callbacks from `call_sub_value` (tree-walk) → `VM::call_react_callback` (executing compiled bytecode while binding the topic via `vm_call_map_block`) #3039. The await/Promise path is reached via a thin `Interpreter::drive_react_subscriptions` bridge (mem::take/VM/restore). **Stage 3 follow-up = the supply `QUIT` handler is also now VM-native**: the 2 sites inside the VM drive loop (the React/Channel quit paths in `vm_react_loop.rs`) that fell back to `self.interpreter.call_supply_quit_handler` (tree-walk `call_sub_value`) were replaced with `VM::call_supply_quit_handler` (executing compiled bytecode via `call_react_callback`). **With this, no callback in the drive loop ever returns to tree-walk.** `Interpreter::call_supply_quit_handler` is used only from the Interpreter's standalone on-demand/tap supply path (`native_supplier_methods`/`native_supply_mut_methods`), which is itself a separate tree-walk runtime and out of scope here. `supply_emit_buffer` remains an Interpreter field (reachable from the VM via `self.interpreter.`; making it global is unnecessary). |

## §2 — True fallbacks in function dispatch (`call_function` / `call_function_fallback`)

| file:line | context | difficulty | blocker / dependency |
|---|---|---|---|
| ~~`vm_var_get_ops.rs` 0-arg term~~ | ~~0-argument user/multi function term~~ | — | **✅ Eliminated (PR3)**: cold fallback moved to unified compiled-first (added OTF compile) |
| ~~`vm_var_get_ops.rs` pkg-qualified~~ | ~~`Module::func` in term position~~ | — | **✅ Eliminated (PR3)**: same as above |
| `vm_call_func_ops.rs` builtin-shadow (slip + normal, 2 sites) | user sub shadows a same-named builtin / **non-builtin module / dynamically registered single sub** | partially eliminated | **✅ The compilable single-candidate portion was OTF-compiled in ③ PR-2** (builtin-shadow uses strict `def_is_otf_compilable`). **✅ Non-builtin singles also OTF'd (§D, #TBD)**: new `def_is_otf_compilable_module_single` (default-allow, name-cache safe = no same-named builtin) plus a conservative gate (`module_otf_body_needs_interpreter` excludes state/sigilless/rw/raw/nested-routine/subtest/CATCH/CONTROL/phaser/start/EVAL/test-assertion). module sub 100%→0%. pin `t/module-sub-otf-dispatch.t`(14). proto/multi/complex sigs keep the fallback |
| `vm_call_func_ops.rs` multi-dispatch fork / final else | non-proto multi / `call_function` terminal | partially eliminated | **✅ Unambiguous/OTF-compilable/non-state candidates of non-proto multis were OTF-compiled in ③ PR-4, and where-constrained candidates were also OTF'd in §D** (ambiguity/state-alternate/proto-body/terminal keep the fallback) |
| `vm_call_func_ops.rs` proto sub dispatch (trivial body) | `{*}` dispatch of `proto foo {*}` / bodyless proto | partially eliminated | **✅ Eliminated (#3541)**: at the VM call site, `vm_resolve_trivial_proto_candidate`→`compile_and_call_function_def`. Fully bypasses the tree-walk proto body + `__PROTO_DISPATCH__` + candidate `run_block`. Proto sigs are gated via `method_args_match`. Non-trivial bodies / non-OTF candidates keep the fallback |
| ~~`def_is_otf_compilable` where exclusion~~ | ~~multi/proto/single candidates with where constraints~~ | — | **✅ Eliminated (§D, #3543)**: removed `where_constraint.is_none()` from `def_is_otf_compilable`, so where-constrained candidates are OTF-compiled too. The winner has already had its where evaluated via `args_match_param_types` at resolve time = the resolved def satisfies the where. Compiled binding re-validates where + merges the `&name` captured env, byte-identical. The light-call fast path keeps the where exclusion. fallback 77.8%→0%. **The same PR sorts `resolve_all_multi_candidates` in specificity order** (fixes the hash-seed flake where nextsame/callsame picked a broader candidate first among multiple matching candidates; defer-next.t). pin `t/multi-where-otf-dispatch.t`(20) |
| `def_is_otf_compilable` default exclusion | multi/proto/single candidates with default param values | DEFERRED | **PR #3546 closed**: plainly removing `default.is_none()` is green on make test + all of S06/S12, but the release make roast regressed env.t/system.t/cur-current-distribution.t. Root cause = Test::Util's `our sub run(Str $code, Str $input='', *%o)` shadows the builtin `run` → OTF compile on the builtin-shadow path + name-cache pollution → in stateful contexts like subtest, subsequent core `run` calls get mis-bound. A default-param variant of the PR-2 builtin-shadow hazard. Safe implementation = keep the default-param exclusion on the builtin-shadow single-candidate path and allow only multi / non-builtin singles (needs full roast validation). fallback 85.7%. impl+pin preserved = branch multi-dispatch-default-otf |
| `vm_call_dispatch.rs` catch-all | `call_function_compiled_first` terminal (nearly exhausted by measurement = see PR-6 note below) | HARD | ③ (remaining = lexical-alias-to-builtin / `__mutsu_*` internal / concurrency [lever B] / no-match error generation) |
| ~~`vm_dispatch_helpers.rs` Routine call_function~~ | ~~function resolution of Routine values~~ | — | **✅ Eliminated (③ PR-1)**: 3 sites moved to unified compiled-first (Routines with builtin names keep builtin priority) |

## §C — CARRIER (not elimination targets; documented and kept. Finalized in ④)

| file:line | kind | reason |
|---|---|---|
| `vm_call_method_compiled.rs` pseudo-method (non-mut/mut) | MOP reflection | DEFINITE/WHAT/WHO/HOW/WHY/WHICH/WHERE/VAR are reflection; no bytecode form |
| `vm_call_method_compiled.rs` ^metamethod (non-mut/mut) | MOP | `Foo.^bar` metamethods are MOP-owned |
| `vm_register_ops.rs` `.VAR` + `trait_mod:<is>` | container reflection + metaprogramming hook | `.VAR` reflection + user trait handlers |
| `vm_register_ops.rs` `trait_mod:<is>` sites | metaprogramming hook | applying is traits to subs/types/variables (multiple sites) |
| `vm_var_get_ops.rs` pseudo-package | reflective scope resolution | `SETTING::`/`OUTER::`/`CALLER::`/`DYNAMIC::` |
| `vm_var_get_ops.rs` test-function | mode state | Test harness dispatch for `make-temp-dir` etc. |
| `vm_var_get_ops.rs` call-chain | MOP dispatch stack | `callsame`/`nextsame`/`callwith`/`nextwith`/`nextcallee`/`lastcall` |
| `vm_call_func_ops.rs` / `vm_call_dispatch.rs` carrier branches | EVAL sub-VM | `EVAL`/`EVALFILE` are compile→sub-VM execution. Already runtime-detected via `is_interpreter_carrier_function` |

## Progress log

- **2026-06-08 (PR-1, #2755 merged)**: starting point of ①. Made all fallbacks visible with `// TODO: compile to bytecode` / `// CARRIER:`
  and created this ledger. Also one EASY elimination: replaced the raw `interpreter.call_method_with_values` for `succ`/`pred`
  (`increment_value_smart`/`decrement_value_smart` in `vm_var_assign_ops.rs`) with the unified compiled-first dispatch
  `try_compiled_method_or_interpret` (2 sites eliminated from §1).
- **2026-06-08 (PR-2)**: eliminated 2 more value-receiver raw `call_method_with_values` sites with the same technique →
  the `vm_smart_match.rs` smartmatch key-method and the `vm_dispatch_helpers.rs` Routine method-dispatch branch
  (`&?ROUTINE.dispatcher()(self,…)`) now go through unified compiled-first dispatch. User-defined methods run as compiled
  bytecode; only native/reflective reach the shared terminal. Added t/smartmatch-method-dispatch.t. All S03-smartmatch pass.
  (Note: fat-arrow `$o ~~ (k => v)` returning False is a separate parse bug hit before the Pair branch is reached, out of scope for this work. Colon pairs are fine.)
- **2026-06-08 (PR-3)**: moved the 2 cold term-position fallbacks of §2 (0-arg term / pkg-qualified in `vm_var_get_ops.rs`)
  from raw `interpreter.call_function` to the unified entry `call_function_compiled_first`, and added OTF compile for
  simple user subs (interpreter is terminal only). Also corrected the native-method row to **③-blocked**
  (the "can be done individually" assessment was wrong because `native_io_*` requires interpreter-owned state such as file handles).

- **2026-06-08 (② extraction/read/write-through complete, #2760-2772 + this PR)**: VM ownership of the declaration registries (phase ②)
  completed through PR-A (extraction; all fields into `Registry`) → PR-B (lookup/MRO/type matching as `impl Registry` methods) → PR-C
  (cleanup of the `register_*_decl` write-through). Primary source: `docs/vm-registry-ownership.md`. PR-C confirmed the
  **registration path = CARRIER nature** (`register_class_decl`/`register_sub_decl`/`register_enum_decl`/`register_role_decl` execute
  `eval_block_value`/`run_block_raw`/`call_function` during registration = class bodies, trait handlers, attribute defaults, enum variant values,
  and parameterized role bodies = execution triggers), and replaced `registry()`/`registry_mut()` with a **re-entrancy-detecting wrapper guard**
  enforcing at runtime, in debug builds, the discipline "never hold an RwLock guard across re-entry" (panics with position info right before a
  blocking call on same-lock reacquisition; keyed by lock address so the legitimate case of holding a different registry concurrently is allowed).
  This puts in place the structural prerequisite (②) for eliminating the §2 function dispatch fallbacks
  (`vm_call_func_ops.rs` multi/sub resolution, `vm_dispatch_helpers.rs` Routine). The remaining §1/§2 require ③ (state ownership transfer).

- **2026-06-08 (③ PR-1, Routine dispatch)**: finalized the ③ design in `docs/vm-state-ownership.md` (unlike ②, ③ is
  **fallback-elimination-driven** = the shared-handle approach is not viable; env's terminal state is a plain field. Built the state×coupling map). As the first
  slice, moved the 3 raw `interpreter.call_function` calls (qualified / bare / terminal) of the Routine value dispatch (vm_call_on_value)
  in `vm_dispatch_helpers.rs` to the unified entry `call_function_compiled_first`, so user-defined sub/multi/proto run as compiled bytecode.
  **Preserving builtin priority**: `&SETTING::...::not` resolves to `Routine{GLOBAL, "not"}` (accessors.rs) = it intends the builtin even when a
  user `sub not` shadows the name. With the `Interpreter::is_builtin_function` guard, only Routines bearing builtin names keep `call_function`'s
  builtin priority (a plain user `&not` is a `Value::Sub` and does not reach the Routine branch). The first naive conversion regressed
  `S02-names/SETTING-6e.t` (user `sub not` + `&SETTING::not`), which proved the point. pin = `t/routine-value-dispatch.t`(10).
  S06/S02-magicals/S02-names/S03-smartmatch whitelist 137 files green.

- **2026-06-08 (③ PR-2, builtin-shadow fork)**: for the function dispatch fork where a user-defined sub shadows a same-named builtin
  (2 sites: the `exec_call_func_op` normal path + `exec_call_func_slip_op` in `vm_call_func_ops.rs`,
  `user_function_matches_call` branch), lowered to OTF compile (`compile_and_call_function_def`) and bytecode execution only when the resolved
  def is a **plain single candidate and compilable**. It does not fall to the native arm (so as not to pick up the shadowed builtin).
  Compilability is judged by the new helper `def_is_otf_compilable` (consolidating the guards of the existing OTF branches). **Found and fixed regressions in 2 stages**:
  ① initially, proto/multi also got their single `resolve_function_with_types` candidate OTF-compiled, breaking candidate dispatch of proto'd multis
  (whitelisted S06-multi/proto, subsignature, type-based went mid-file abort = exit 255/Failed:0) →
  added the `!has_proto && !has_multi_candidates` guard. ② **the `user_function_matches_call` branch is not builtin-shadow-only: it receives
  all "args-matching user subs absent from compiled_fns (module/dynamically registered)"**. `def_is_otf_compilable` cannot capture `when` control
  flow etc. inside nested `sub` declarations, and when Test::Util's `is-deeply-junction` (nested `junction-guts` + `when`) was
  OTF-compiled, the `when`-succeed escaped the whole function and dropped the eigenstates (`t/test-util-is-deeply-junction.t`
  / `throws-like-any` regressions) → **limited to actual builtin shadows via the `Interpreter::is_builtin_function(name)` guard**
  (non-builtin module subs stay tree-walk as before). pin = `t/builtin-shadow-dispatch.t`(9). S06 whitelist 86 files green
  (S32-str gb2312/shiftjis also fail on main = pre-existing environment-dependent). Remaining: the non-proto multi fork (requires VM-side multi candidate resolution),
  the catch-all terminal (③), tree-walk of non-builtin module subs (requires expanding compiled_fns or a safe generic OTF gate).

- **2026-06-09 (③ PR-3, §1 catch-all = user-method compile-on-demand + correcting the actual picture)**: at the dispatch chokepoints of the §1 catch-all
  (`try_compiled_method_or_interpret` line 457 / `try_compiled_method_mut_or_interpret` line ~919 in `vm_call_method_compiled.rs`), added
  **compile-on-demand for resolved user method defs whose `compiled_code == None`** (new helper `populate_uncompiled_method`:
  idempotently compiles the canonical registry via `compile_class_methods`/`compile_role_methods` → re-resolves → executes bytecode via `dispatch_compiled_method`.
  If the owner is not a user class/role or the body is empty, returns `None` to preserve the interpreter fallback). pin `t/method-otf-dispatch.t`(14).
  **The biggest yield was correcting the measured reality**: measuring the catch-all reach set with `MUTSU_VM_STATS=1` + a probe showed the ledger's claim that
  "catch-all = the remaining primary tree-walk (user methods)" was **wrong**. In reality:
  ① **normally declared methods (including multi methods), submethods, role composition, inheritance, and `.^add_method` are all bytecode-compiled at
  registration time via `compile_class_methods`** (`.^add_method` inherits the `compiled_code` of the method-literal Sub,
  `methods_classhow.rs:640`). ② The Instance traffic reaching the catch-all is **only native coercion (`Exception.Stringy`) /
  MOP (`.does`/`.^does`) / role-qualified (`WithAttr.AccessesAttr::meth` resolves to None and never enters the resolved block)** = dependent on ③/builtins/a separate resolution bug. ③ The only compile-gap where `populate_uncompiled_method` actually fires is
  **`.^add_multi_method`** (`compiled_code: None` fixed, `methods_classhow.rs:702`) plus defensive coverage of future sites of the same kind.
  So this PR **reliably bytecode-compiles the user-method portion of the §1 catch-all** (the remainder is native/MOP/separate resolution bugs).
  S12/S14/S06-multi/metamodel whitelist all green, make test PASS. **Lesson: the fallback counters count "via the interpreter bridge",
  not "tree-walk"** (the `run_block_raw` on the far side of the bridge is actually bytecode via the VM compile ping-pong).

- **2026-06-09 (③ PR-4, §2 non-proto multi fork)**: for the non-proto multi fork of `vm_call_func_ops.rs::dispatch_func_call_inner`
  (`has_multi_candidates && !has_proto`), like PR-2's builtin-shadow style: resolve the winner via
  `resolve_function_with_types` (VM-accessible thanks to ②) → if `def_is_otf_compilable` and the body is non-state,
  OTF compile / bytecode-execute via `compile_and_call_function_def`. **On the function path, ambiguity is expressed as `None`+`pending_dispatch_error`**
  (`dispatch.rs` choose_best_matching_candidate), so `Some(def)` = unambiguous winner (a separate mechanism from the method side's
  `dispatch_ambiguous` flag). A stale `pending_dispatch_error` is cleared with `take` before resolving (same etiquette as the interpreter's
  `resolve_function_with_alias`). ambiguity/where/default/code-param/no-match/proto/terminal go through
  `call_function_fallback` as before (throwing the canonical `X::Multi::Ambiguous`/`X::Multi::NoMatch`). **nextsame/callsame/callwith work correctly
  even from compiled candidates** (because `compile_and_call_function_def` pushes the same `push_multi_dispatch_frame` as the interpreter;
  confirmed by measurement = excluding redispatch was unnecessary). **Handled 2 pitfalls**: ① **name pollution of otf_call_cache** —
  conditioned the name-cache insert of `compile_and_call_function_def` on `!has_multi_candidates_cached`, plus the same guard on the lookup side
  (prevents the type-blind name cache from wrongly reusing `f(5)`→Int candidate for a subsequent `f("hi")`. The fingerprint-keyed
  `otf_compile_cache` is per-candidate and safe, so it stays). ② **shared state of signature alternates** (`(A)|(B){ state $x }` shares
  one cell via state_group at compile time, but OTF's body fingerprint = per-alternate-sig key would give separate states) →
  **multi bodies containing state declarations are excluded from OTF** (new helper `function_body_declares_state` recursive scan. Found and fixed
  the `t/multi-signature-alternates.t` regression). The slip path (`exec_call_func_slip_op`) multis are out of scope for this PR because
  **an existing separate bug makes `|ms()` return Nil** (follow-up). **③ Also fixed an existing flaky bug**: `push_multi_dispatch_frame`
  (`accessors.rs`) determined the current of the callsame/nextsame "remaining candidates" frame via `resolve_all_matching_candidates().first()` =
  **HashMap-order first match**, so when the narrowest candidate was declared later, callsame redispatched to the wrong (or same) candidate,
  flaking ~50% depending on the process hash seed (whitelisted files like `roast/S06-advanced/callsame.t` failed intermittently; the substance of the
  CI #2788 failure). Fixed to **determine current with the same deterministic winner as the interpreter's inline frame (`resolve_function_with_alias`)**
  (deterministic across all VM paths = Path A / otf cache / compile_and_call at once). callsame now works correctly even from OTF candidates,
  so the redispatch exclusion became unnecessary. pin `t/multi-otf-dispatch.t`(25, including callsame-when-winner-declared-last).
  Measured multi-probe fallback 5→2 (remaining 2 = the nextsame/callsame builtins themselves). S06/S12/S14 whitelist all green (10x deterministic),
  make test PASS. **Lesson: "my PR's CI failure" can be an existing flake surfacing — pin down seed-dependent nondeterminism by measurement.**
  **Also found and fixed 1 more regression (PR-2's rut)**: the multi fork OTF-compiled **native Test routines** (is-eqv/is-deeply etc. — Rust implementations,
  but a multi stub is registered in the registry), bypassing the native handler and deterministically breaking `S16-io/words.t` and `S32-io/slurp.t`
  (via is-eqv). Resolved by adding the same `!is_interpreter_handled_function(name)` guard as the non-builtin OTF path to the multi fork
  (is-eqv correctly routes to call_function_fallback → the native test handler). Confirmed via local full make roast PASS.

- **2026-06-09 (③ PR-5, §1 catch-all = making plain `@`-array mutators VM-native)**: added temporary instrumentation to `MUTSU_VM_STATS` recording the
  catch-all receiver type names (`Class.method`), and **measured the catch-all reach set** over the whole whitelist sample.
  PR-3 had estimated "the remainder is native/Buf/Failure", but **the reality was not Buf/Failure but array mutators + iterator protocol + coercion**
  (top: `Package.new`=38698 [③ constructor; ③-blocked by user BUILD/attributes], `Array.append`=16721, `Array.shift`=5796,
  `Any.AT-POS`=5795, iterator `pull-one`/`skip-one`/`push-exactly` family, `Array.splice`=381, `List.Set/Bag/Mix` coercion …).
  Excluding `Package.new`, **the largest tractable category = array mutators** (append+shift+splice ≈ 22900). This PR adds
  `try_native_array_mut` to `vm_call_method_mut_ops.rs::exec_call_method_mut_op`, making **`append`/`prepend`/`unshift`/`pop`/`shift` on plain
  untyped `@`-arrays (`ArrayKind::Array`)** VM-native (`env.get_mut` + `Arc::make_mut`, identical semantics to the interpreter's primary branch;
  empty pop/shift uses `make_empty_array_failure_what(..,"Array")`).
  **Conservative fallthrough**: typed (`var_type_constraint` Some) / shaped / lazy (kind other than `Array`) / shared
  (`shared_vars_active`) / metadata-bearing (`container_type_metadata` Some) / non-env-bound receivers stay on the interpreter.
  **Pitfall: Arc-ptr keying aliasing of type_metadata** (🟣 a known first-class-container hazard) — a new Arc pointer obtained by a `make_mut` reallocation
  collided with a stale `array_type_metadata` entry of a freed typed array, mis-typing an untyped array as `Array[Int]`
  (reproduced only in full-file context, intermittently, allocator-dependent). Since the native path guarantees untyped, we defensively remove
  the stale entry via `unregister_container_type_metadata` on the post-mutation env array (safe = no live other array can hold the same pointer).
  pin `t/native-array-mut.t`(31, including the aliasing case). S32-array whitelist all green, make test PASS, array-related whitelist 156/156.
  **Remaining catch-all**: `Package.new` [③], `Any.AT-POS`/iterator protocol [value types/iterator state], coercion [candidate for lowering to builtins], splice.

- **2026-06-09 (③ PR-6, §2 measuring the catch-all terminal + lowering the junction constructors to builtins)**: added `END:`-prefixed instrumentation to the
  `call_function_compiled_first` terminal (`call_function` final else) at `vm_call_dispatch.rs:79` and **measured the terminal reach set** over the whole
  whitelist sample. Conclusion: **the terminal is nearly exhausted** (thanks to PR-1..4 plus lines 63-67's "every resolvable def gets OTF-compiled",
  user functions no longer reach the terminal). Across the sample, terminal reach is diffuse and small; the breakdown: ① the `any`/`all`/`one`/`none`
  junction constructors (pure builtins; lowered in this PR), ② **name-based calls of lexical `&`-variables** (the *largest* terminal-residual category.
  `-> &op { … op(…) }` = S03-operators/set_*.t's `END:op` [348 each etc.] binding set-operator Callables as parameters and calling them,
  and `my &junction = ::("&any"); junction(|$_)` = S03-junctions/autothreading.t's `END:junction=56`. Since the bound Callable
  [user sub / operator / builtin Routine] is called by bareword name, lexical resolution at the terminal = works correctly. Future slice: detect
  lexical `&`-var bindings on the VM side and route to the existing Routine/compiled dispatch), ③ `__mutsu_*` internals (CAS etc.; concurrency is lever B),
  ④ no-match error generation (`notthere` = throwing the canonical exception at the terminal is correct).
  **No high-traffic builtins remain at the terminal** (`split`/`index`/`comb` etc. are already native or take the Instance-guard fallback via the other
  4 sites [vm_call_func_ops]). This PR lowers the only pure-builtin category = **junction constructors into `builtins/functions.rs::build_junction`**
  (including the one-arg flatten rule; no state used), routes any/all/one/none at all arities through `native_function`, and delegates the interpreter's
  `builtin_junction` to the same fn, **resolving the duplicate implementation** ([[feedback_dedup_over_perf]]/[[feedback_placement_audit]]).
  Junction construction is type-independent and safe, so the Instance-arg guard of `try_native_function` is bypassed for any/all/one/none only
  (`any($instance)` is native too). pin `t/native-junction-ctor.t`(24, including Instance-arg). S03-junctions whitelist all green, make test PASS.
  **Conclusion: the §2 terminal has finished its "high-traffic elimination" phase. The largest remaining category is name-based calls of lexical `&`-vars (works correctly; future candidate for moving into the VM);
  the rest is ③ state ownership [concurrent CAS] / error-generation carrier.**

- **2026-06-09 (③ PR-7, §1 catch-all = making mutable Buf write methods VM-native + lowering to builtins)**: in the same shape as PR-5,
  added `try_native_buf_mut` to `vm_call_method_mut_ops.rs::exec_call_method_mut_op`, making
  **`write-bits`/`write-ubits`/`write-num32|64`/`write-int8..128`/`write-uint8..128` on mutable `Buf` instances** VM-native
  (`overwrite_instance_bindings_by_identity` + `env.insert` = the same writeback as the interpreter's instance-mutate branch. Aliasing
  [multiple variables holding the same instance id] is also observed correctly). **Conservative fallthrough**: type-object receivers
  (`buf8.write-...` is a separate path returning a fresh buf) / immutable `Blob`
  (the interpreter throws "Cannot modify immutable Blob") / malformed arity, offset/bits parse failures stay on the interpreter =
  behavior-invariant. **dedup/placement ([[feedback_dedup_over_perf]]/[[feedback_placement_audit]])**: unified the pure byte transforms
  into builtins = new `src/builtins/buf_bits.rs` (lowered `read_bits`/`write_bits` from `impl Interpreter`, also resolving the read-bits duplicate in
  methods_mut) + moved `buf_write_num.rs`/`buf_write_int.rs` from `runtime/` → `builtins/` (22 call-site path updates).
  The authoritative home of Buf binary read/write transforms is now unified under `builtins/`, and the VM and interpreter share a single implementation.
  Measured: write-bits/write-ubits fallback of `read-write-bits.t` 3123→3 (remaining 3 = type-object form). pin `t/native-buf-mut.t`(23,
  including aliasing / buf growth / Blob dies). S03-buf/S03-operators/S32-container/S02-types/signed-unsigned-native all green, cargo test 458/0.
  (write-int.t is the known pre-existing 128-bit-unsupported blocker = non-whitelisted, unrelated to this change.)

- **2026-06-09 (③ PR-8, §1 = making QuantHash coercion VM-native + lowering to builtins)**: via **spread measurement of fallbacks**
  (whitelist 145 files; distinct file counts), confirmed the next tractable pure category after `new` (63 files, ③ ctor) =
  **`.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`/`.MixHash` (~11 files)** and started on it. Added `try_native_quanthash_coerce` just before
  the catch-all in `vm_call_method_compiled.rs` (same position as PR-7/PR-5 = after user-method resolution, so no shadowing).
  **dedup/placement lowering**: lowered `dispatch_to_set`/`dispatch_to_bag_with_what`/`dispatch_to_mix` (+ helpers `pair_weight`/
  `mix_pair_weight`/`mix_add_item_with_keys`/nested `add_item`/`flatten_into`) from `&self` methods of `impl Interpreter`
  (actually self-independent; only mutually recursive) into **pure free fns in `src/builtins/quanthash_coerce.rs`**
  (`to_set`/`to_bag`/`to_mix`). The interpreter's `dispatch_method_by_name_2` and `methods_dispatch_new` (mix_pair_weight)
  delegate to builtins = single implementation. `is_lazy_for_coerce`/`is_lazy_for_set_ops` are used in 6 other files, so they stay in runtime
  (made `pub(crate)` and referenced from builtins). methods_collection.rs 961→315 lines. **Conservative fallthrough**: `.MixHash`
  (requires `register_container_type_metadata` = interpreter-owned type metadata registration) / Instance (`__baggy_data__`, user coercion) /
  Package (type object) receivers stay on the interpreter = behavior-invariant. Measured: Set/Bag/Mix fallback in set-op tests → 0.
  pin `t/native-quanthash-coerce.t`(26). All 29 whitelisted set/bag/mix files green, cargo test 458/0. (The known fails in set.t/bag.t are the
  pre-existing ones listed in BLOCKERS.md [bag.t 215=BigInt weight, 252=Foo instance union, set.t 226=typed-hash bind], unrelated to this change.)
  **Next candidates: making the iterator protocol (pull-one/skip-one/push-exactly) VM-native, or native `AT-POS`. The main prize is `new` (③ ctor).**

- **2026-06-09 (③ PR-9, §1 = making the simple array-backed Iterator protocol VM-native)**: started on the iterator protocol, the runner-up in PR-8's spread
  measurement (pull-one=2466/skip-one=2130 etc., 4-5 files, high counts). Added `try_native_iterator` next to
  `try_native_buf_mut` in `vm_call_method_mut_ops.rs`, making `pull-one`/`skip-one`/`skip-at-least`/`skip-at-least-pull-one`/`sink-all` on
  **self-contained `Iterator` instances of `items`(Array)+`index`(Int)** VM-native (advance index →
  identity writeback via `overwrite_instance_bindings_by_identity`(env) + `overwrite_instance_in_locals`(locals) = identical to the interpreter's
  mutating iterator dispatch. Aliases and sub local slots also advance correctly). **Key discovery**: `$it.pull-one`
  **compiles to CallMethodMut** (`expr_method.rs:110`: variable receivers are all CallMethodMut) = initially placed on the non-mut path it never fired;
  moving it to the mut path resolved this. **Conservative fallthrough**: squish iterators (`squish_source` = user `as`/`with` callbacks) / lazy
  iterators (gather/coroutine = `is_lazy` attribute; interpreter coroutine pull rather than a materialized items snapshot) / push-* (needs array-identity
  writeback to an external buffer arg) / count-only/bool-only (treated as predictive) stay on the interpreter = behavior-invariant.
  **Found and fixed 1 gather regression**: the first version also grabbed `is_lazy` iterators, making `gather{...}.iterator.pull-one` return IterationEnd immediately →
  resolved with the `is_lazy` exclusion (after exclusion, interpreter fallthrough = pre-existing behavior). Measured: pull-one fallback of List/Array/finite-Range
  iterators → 0. pin `t/native-iterator.t`(18, including aliases/sub locals/Range). S07-iterationbuffer/*-iterator all green, cargo test 458/0.
  (gather.t's Failed:1 is the pre-existing take-rw [test 38] listed in BLOCKERS.md, unrelated to this change.)
  **Next candidates: native `AT-POS`, broad §1 beyond coercion, or design of the main prize `new` (③ ctor).**

- **2026-06-09 (③ PR-10, §1 catch-all = making plain `@`-array `splice` VM-native)**: as a sister to PR-5's
  `try_native_array_mut` (append/prepend/unshift/pop/shift), added `try_native_array_splice` to `vm_call_method_mut_ops.rs`,
  making **the simple form of `splice` on plain untyped `@`-arrays (`ArrayKind::Array`)** VM-native.
  Behavior-invariant with the same `drain`+`insert` as the `splice` branch of the interpreter's `methods_mut.rs` (returning removed elements via
  `Value::real_array`) + `Arc::make_mut` writeback + defensive removal of stale type-metadata after make_mut reallocation.
  **Conservative fallthrough** (`None`→interpreter): offset/count other than plain non-negative `Int` (WhateverCode/`Whatever`/
  `Str`/`Num`), offset out of range (`X::OutOfRange`), negative count (`X::OutOfRange`), lazy replacement (`X::Cannot::Lazy`),
  typed (`var_type_constraint` Some) / shaped / shared (`shared_vars_active`) / metadata-bearing arrays. **Verified**:
  all 16 forms of splice_check match raku; the `@j := @i` alias case gives **`[1 2 3 4]` on the interpreter splice path too (fallthrough via
  WhateverCode offset)** = a pre-existing container-identity gap (🟣 first-class containers Phase 2), not splice-specific, so behavior unchanged.
  pin `t/native-array-splice.t`(28, including replacement flattening, tail splice, count clamping, independent removed
  list, fallthrough X::OutOfRange/X::Cannot::Lazy). Zero not-oks in the untyped region of `S32-array/splice.t` (first 60 subtests);
  push/pop/shift/unshift/S03-binding/arrays/S09-multidim/methods all green, cargo test 458/0, make test PASS.
  (splice.t as a whole is a non-whitelisted pre-existing fail = the `array[int]`/`array[int8]` typed-array metadata problem; the native
  path always bails on typed arrays = interpreter kept, so the count is unchanged.)

- **2026-06-10 (③ PR-11, §1 = extending native default construction to typed `$` attributes)**: extending the native-construction coverage of the largest §1
  catch-all category, `Package.new` (constructors). `is_native_default_constructible`/`build_native_default_instance`
  (`methods_object.rs`; shared by the VM's `try_compiled_method_or_interpret` and the interpreter's `dispatch_new`), which previously covered
  **only untyped `$` attributes**, was extended to **`$` attributes with simple class constraints** (`has Int $.x` etc.). All divergences are delegated
  to the interpreter via **conservative fallthrough**, keeping it behavior-invariant: ① provided value fails the type check (`!type_matches_value`) → None
  (interpreter does `X::TypeCheck::Assignment`/coercion), ② typed attribute with neither arg nor default → None (an uninitialized typed attribute is the
  **type object** = `Int`, not Nil. Synthesis is left to the interpreter), ③ typed default value fails the type check → None. The gate
  **excludes native/coercion/parametric types** (`is_simple_native_ctor_constraint` = starts uppercase, no `(` `[`); native
  lowercase (`int`/`num`/`str`, defaults 0/"") stays on the interpreter. **An important pitfall caught by roast**: the source of truth for type constraints is
  the `attribute_types` map, not the constraint slot of the `ClassAttributeDef` tuple (the tuple side is None) = reading the tuple initially meant the type check
  didn't apply and `Int $.x = "str"` was accepted (regression) → fixed to derive from `collect_attribute_type_constraints`. In addition, classes with the
  **`is built` trait / MOP `Attribute.set_build`** (custom build closures; the `attribute_built` map + registry `attribute_build_overrides`) cannot be
  pure-data constructed, so they are excluded by the gate (caught and fixed the set_build runtime type-check regression in `S12-attributes/defaults.t`).
  pin `t/native-ctor-typed-attrs.t`(30: provided values / type-object uninitialized / Str/Real/mixed / inheritance / mutually-referencing defaults / subset types /
  type-mismatch dies / native int stays interpreter / loop construction). S12/S14/S03-binding/roles whitelist 184 files all green, cargo test 458/0.
  **Remaining: typed `@`/`%` attributes, required, where, coercion types, native types still on the interpreter (a conservative design that stops short of the main prize, ③ env execution).**

- **2026-06-10 (③ PR-12, §1 = extending native construction to untyped `@`/`%` attributes)**: continuing PR-11, extended native default
  construction to **untyped `@`/`%`-sigil attributes** (`has @.items`/`has %.map`). **The target of behavior-invariance is the current interpreter,
  not raku** (measured: `items => 5`→`5` [doesn't wrap the scalar]; invalid elements of typed `@.nums`→NODIE [no element type
  check at construction] = pre-existing behavior differing from raku). Reuses the interpreter-shared `coerce_attr_value_by_sigil`, so
  provided-value coercion (List/Range→Array, array-of-Pairs→Hash) matches exactly. Unprovided default = empty Array/Hash (no type-object
  synthesis needed = simpler than typed `$`). **Conservative fallthrough**: typed elements (`Int @.nums` = requires container type metadata registration;
  the Arc-keying hazard) / the `is Type` trait (`class_attribute_is_types`) / **`@`/`%` with a default_expr** (shape is
  encoded in the default = `has @.a[2]`; falls through regardless of whether a value is provided) stay on the interpreter. **roast caught a shaped pitfall**:
  initially only "has default + unprovided" fell through, but when a shaped `@.a[2]` **was provided**, coercion lost the shape
  (regression of `.a.shape`=(2,) in `S12-introspection/attributes.t`) → fixed to "if `@`/`%` has a default_expr, fall through unconditionally".
  pin `t/native-ctor-array-attrs.t`(27: empty default / list, array, range provided / hash, pairs provided / mixed typed-$ / inheritance / defaulted
  fallthrough / typed-element fallthrough / loop construction). S12/S14/binding/roles 184 files all green, cargo test 458/0, make test PASS.
  **Remaining: typed `@`/`%`, `is Type`, shaped, required, where, coercion types, native types still on the interpreter.**

- **2026-06-10 (③ PR-13, §1 = extending native construction to `is required` attributes)**: continuing PR-11/12, extended native default
  construction to **`is required` attributes** (the previous gate rejected with `!is_required`). Provided required attributes are built natively
  (typed `$` with type check included); **unprovided required attributes fall through** (interpreter raises `X::Attribute::Required`).
  Removed `!is_required` from the gate and added a "required and not provided → None" check at the start of build. **Behavior-invariant**:
  unprovided required `$` dies in the interpreter (matches raku); unprovided required `@`/`%` is **not enforced** by the interpreter
  (NODIE = a pre-existing gap differing from raku, but native falls through and matches interpreter behavior). pin
  `t/native-ctor-required-attrs.t`(14: all required provided→build / unprovided `$`→dies / type mismatch→dies / required `@`/`%`
  provided→build / inherited required). S12/S14/binding/roles 184 files all green, cargo test 458/0, make test PASS.
  (While writing the pin, hit 2 pre-existing parser limitations: **consecutive bare blocks `} { ` unparseable** and **class name `Q` colliding with the quote operator**
  = worked around by restructuring the test to top-level structure + non-colliding names. Unrelated to this slice.)
  **Remaining: where, coercion types, native types, typed `@`/`%`, `is Type`, shaped still on the interpreter.**
- **2026-06-14 (③ ctor, §1 = extending native construction to definedness smiley `:D`/`:U`/`:_` attributes)**: the ctor slice following TWEAK(#3028)/
  where(#3030)/BUILD(#3032). Attributes like `has Int:D $.x`, previously rejected by the gate's `attribute_smileys.is_empty()`, are now built natively.
  Removed the smiley check from the gate; on the build path, `enforce_attribute_smiley_constraints`
  (the same helper as the full path) runs **at the same position as where = once post-assembly/pre-BUILD + a recheck post-BUILD**.
  **Strictly matches the interpreter baseline (not raku)**: ① `:U` provided defined / `:D` provided undefined dies
  (the "default value of attribute" message = the interpreter's existing behavior. raku says "assignment to", but that pre-existing difference is out of
  scope for this slice), ② **BUILD violating the smiley → dies (post-BUILD recheck, matching full path 3929)**,
  ③ **TWEAK violating the smiley → does not die** (the interpreter does not recheck smileys post-TWEAK nor check on assignment,
  so native does not recheck either = baseline match. raku dies). A bare `:D` (no default, no required) is rejected at
  parse time with `X::Syntax::Variable::MissingInitializer` and never reaches construction. pin
  `t/native-ctor-smiley-attrs.t`(21: `:D`/`:U`/`:_` × default/required/provided/inheritance/mixed/BUILD-dies/TWEAK-lives/
  Str:D). Construction-related whitelist 118 files all green including S12-attributes/smiley.t(54) + S12-class/attributes-required.t(11),
  cargo test 461/0, make test PASS. **Remaining: coercion types, native types, typed `@`/`%`, `is Type`, shaped, same-name redeclaration,
  does-Role attributes, CUnion, custom BUILDALL/new still on the interpreter.**

- **2026-06-14 (③ ctor 2nd wave = post-assembly phase approach, #3028–3036)**: broke the "pure-data only" principle and, after pure-data
  assembly, ran **submethod execution / constraint validation / role mixin as post-assembly phases**, making user-code-running shapes
  native too. Extracted each phase of the `.new` main path into shared helpers (`run_tweak_phase`/`run_build_phase`/`enforce_attribute_where_constraints`/
  `apply_attribute_does_role_mixins`), called from both interpreter and native to guarantee byte-identical. **TWEAK(#3028)** (`TWEAK(:$y)`
  argument passing = the two constructor lineages handled it differently) → **where(#3030)** (enforce both before and after TWEAK = assignment-time semantics) →
  **BUILD(#3032)** (`fail`→Failure / custom BUILD suppresses named-arg auto-assignment / positional rejection = also fixed the pre-existing `S.new("pos")` bug) →
  **is-rw(#3034)** (fixed the bug where `ClassAttributeDef` pos3=is_rw was misread as is_required; unprovided is-rw becomes native +
  `@`/`%` required enforcement also correct) → **does-Role(#3036)** (mixin the role into the attribute value; a documented raku-non-compliant approximation, so
  validated native==interpreter). Individual pin tests (`t/native-{tweak,where,build,isrw,doesrole}-construct.t`). S12/S14 whitelist all green, make test PASS.
  **★ Re-measurement (important)**: after the 5 ctor slices, the whitelist sample's `new` method fallback is nearly unchanged (5230→5225). **Most (4686) is
  `Buf.new` in a single test `S03-buf/read-write-bits.t` = a built-in type constructor** (`is_native_default_constructible` only targets user-defined
  `registry().classes`, so it is correctly out of scope). **= User-class ctor native-ification is complete. Further ctor shaving will not move the `new`
  fallback count.** The next real targets are other categories: **built-in type `.new`** (Buf.new etc.) / **`name`=3257 (MOP)** /
  **`op`=1418 (Routine-value lexical &-var `&infix:<(|)>` binding, the exclusion from this round's Sub/WeakSub-limited fix)** / iterator push-* protocol
  (Track B) / coercion (lowering to builtins). Remaining hard ctor cases: required-after-BUILD / unset class-typed + BUILD / `is built`/
  MOP set_build / BUILDALL / custom new / CUnion.
- **2026-06-23 (§1 = making `.MixHash` coercion VM-native)**: a measurement-driven slice of §D (state ownership). Measuring the dominant catch-all
  categories with `MUTSU_VM_STATS` showed the most frequent was `.MixHash` (the only remainder of QuantHash coercion), which PR-8 had
  **explicitly excluded**. PR-8's exclusion reason was "`.MixHash` registers container type metadata = interpreter-owned state", but
  **this is stale since #2952–2985's embedding of container-value metadata** = the type metadata of the `Value::Mix` produced by `.MixHash`
  (`value_type=Real`/`declared_type=MixHash`) is **embedded in the Mix's `Arc<MixData>`, not an interpreter side table** (the `embed_type_info!` path of
  `tag_container_metadata`; only the `other =>` branch for Instance etc. hits the side table `register_container_type_metadata`).
  ∴ `.MixHash` is a pure value op touching no env/state at all and can be made native in the same shape as `.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`.
  Added the new pure fn `builtins::quanthash_coerce::to_mixhash` (`to_mix(_, "MixHash")` → mutable flip + metadata embedding) to the VM's
  `try_native_quanthash_coerce` (added `"MixHash"` to the method match). **dedup**: the `Mix|MixHash` branch of the interpreter's `dispatch_method_by_name_2`
  now delegates to `to_mixhash` (removed the direct `tag_container_metadata` write) = **1 operation = 1 implementation**. The receiver gate is
  identical to the 5 existing siblings (only list-like is native; Instance/Package stay on the interpreter). Variable receivers (`%h.MixHash`) go to the
  mut path via `CallMethodMut` routing where all 6 siblings uniformly fall back = **exact parity with existing behavior** (a future slice candidate is
  lowering them into the mut path all at once). Behavior-invariant (native==interpreter = same `to_mixhash`). pin `t/native-mixhash-coerce.t`(22,
  PASSing on both raku and mutsu; avoids the `.WHICH` key difference by using Str elements). mix.t 244/244, set.t, categorize green.
  (bag.t 625 "Foo instance union" is pre-existing per BLOCKERS.md and unrelated to this change.)
- **2026-06-23 (§1 = making QuantHash coercion VM-native on the mut path too)**: follow-up to the MixHash slice above. **Variable receivers**
  (`@a.Set`/`%h.MixHash`) compile to `CallMethodMut` and never hit the non-mut path's `try_native_quanthash_coerce`, so all 6
  siblings (`.Set`/`.Bag`/`.Mix`/`.SetHash`/`.BagHash`/`.MixHash`) uniformly fell back to the interpreter at the mut catch-all
  (see the "parity" note of the MixHash slice). Added the same `Self::try_native_quanthash_coerce` to the **mut catch-all** in `vm_call_method_compiled.rs`
  (end of `try_compiled_method_mut_or_interpret`, after `try_native_first`). Coercion returns a **new** Set/Bag/Mix value and
  does not mutate the receiver variable (no writeback needed), so it's exactly the same shape as the non-mut path = behavior unchanged. Instance/Package receivers fall through.
  Measured: fallback of `@a.{Set,Bag,Mix,SetHash,BagHash,MixHash}` 6→0 (remaining `^name` is MOP). pin
  `t/native-quanthash-coerce-mut-path.t`(19, PASSing on both raku and mutsu). mix.t 244/244, set.t, categorize green.
  (bag.t 252 / classify.t 40 [junction classify, non-whitelisted]: confirmed via stash they also fail on main = pre-existing, unrelated to this change.)
- **2026-06-23 (§1 = making `.Map`/`.Hash` coercion VM-native + lowering to builtins)**: continuation in the same shape as QuantHash coercion.
  `.Map`/`.Hash` are **pure value ops** (`dispatch_to_hash_impl`/`dispatch_to_map` are entirely `&self`-independent = they touch no env/registry/
  state, only `Self::` static helpers + `super::utils::`; `.Map`'s declared-type is embedded in the `Value::Hash`'s `Arc<HashData>`,
  no side table used since #2952). **dedup lowering**: moved `dispatch_to_hash_impl`/`dispatch_to_map`/`items_to_hash`/
  `make_odd_number_error` into the new `src/builtins/map_hash_coerce.rs` (pure free fns `to_hash(target, check_odd)`/`to_map(target)`).
  The interpreter's `dispatch_to_hash` delegates; `dispatch_to_map` is removed (the `Map|Hash`
  branch of `dispatch_method_by_name_2` calls `to_map`/`to_hash` directly and the direct `tag_container_metadata` write is removed too) = **1 operation = 1 implementation**. Added to the VM
  `try_native_map_hash_coerce` (both non-mut + mut catch-alls). Receiver gate: only list-like / Hash / QuantHash are native;
  Instance (Match's named captures / `__baggy_data__`) / Package (type objects) / lowercase `.hash` stay on the interpreter.
  Behavior unchanged (`to_map` embeds and keeps identity = `Map.Map === Map`; odd count gives `X::Hash::Store::OddNumber`). pin
  `t/native-map-hash-coerce.t`(21, PASSing on both raku and mutsu). mix.t 244/244, set.t, hash.t, categorize, classify-list green.
- **2026-06-23 (§1 = making the structural branch of `.Seq` coercion VM-native)**: capping the coercion drainage. `.Seq` is
  `dispatch_seq_coercion` (`&mut self`), but **the structural branch (Seq/Array/Slip/Range/bare scalar) is pure**; only
  Supply (on-demand callback-driven) / LazyList (lazy bridge force) / Instance (Buf/Blob byte reads, generic wrap) depend on
  carrier/state. Extracted the pure branch into the new `src/builtins/seq_coerce.rs::to_seq_structural(target) -> Option<Value>`
  (Supply/LazyList/**all Instance** return `None` to the interpreter). The interpreter's `dispatch_seq_coercion` delegates at the top;
  the VM calls it on both non-mut + mut catch-alls when `method == "Seq"` = **1 operation = 1 implementation**. `.Seq` of gather/lazy continues to
  drain correctly via the interpreter carrier. pin `t/native-seq-coerce.t`(16, PASSing on both raku and mutsu). S32-list/seq.t, S17-supply/Seq.t,
  integration/sequence.t green. **This completes the measured drain of the pure-coercion fallback category (Set/Bag/Mix/MixHash/Map/Hash/List/Array/Slip/Seq)**
  = the only remaining coercion fallbacks are `.Setty`/`.Baggy`/`.Mixy` (return type objects; niche).
- **2026-06-23 (§D = making pure lexical `IO::Path` methods VM-native)**: measurement-driven slice after the coercion drain.
  `MUTSU_VM_STATS` **spread measurement (distinct file counts)** showed the largest method-fallback category was `parent`(66 files)/
  `add`(65 files) = **IO::Path path-manipulation methods** spread across 60+ files via the temp-file/dir helpers (`make-temp-dir`/`make-temp-file`)
  (the highest raw count, `AT-POS`=5816, concentrates on the Buf receiver of a single `read-int.t` = spread of only 7 files, so unsuitable).
  Of `IO::Path`'s `native_io_path` (`&mut self`, FS/cwd-dependent), extracted the **pure lexical methods that only manipulate the path string**
  (`parent`/`add`/`child`(non-secure)/`sibling`/`basename`/`dirname`/`volume`/`cleanup`/`parts`/`extension`/`succ`/`pred`/
  `starts-with`/`is-absolute`/`is-relative`/`Str`/`gist`/`IO`/`SPEC`) into the new associated fn `Interpreter::try_io_path_lexical`
  (no `&self` needed = touches no FS/cwd/env at all; uses only static `Self::io_path_*` helpers). **dedup**: `native_io_path`
  delegates to it at the top and the moved arms are deleted = **1 operation = 1 implementation** (the join of `child`/`add` is factored into the shared helper
  `io_path_join_child`; only `child :secure`'s FS-resolve stays in `native_io_path`). The VM calls `Self::is_io_path_lexical_class`
  (limited to built-in `IO::Path`/`::Unix`/`::Win32`/`::Cygwin`/`::QNX`) + `try_io_path_lexical` **immediately before** the `is_native_method`
  bounce on **both non-mut + mut** (placed here rather than the catch-all because the fallback recording happened at the `is_native_method` bounce
  [vm_call_method_compiled.rs:201/1652]). Like coercion, **returns a new IO::Path/string/bool and does not mutate the receiver** = no writeback needed.
  FS/cwd-dependent (`e`/`f`/`slurp`/`spurt`/`absolute`/`relative`/`resolve`/`CWD`/`raku`), numeric coercion, and `child :secure` return
  `None` and stay on the interpreter = behavior-invariant. Measured: fallback of `$p.{parent,add,basename,dirname,sibling,…}` → 0
  (what remains is `Str.IO` coercion [a different operation, Str receiver] and `IO::Path::*.new` [③ ctor]). pin `t/native-io-path-lexical.t`(40,
  literal `.IO` + variable receiver [mut path] + Win32 subclass round-trip; PASSing on both raku and mutsu). The 2 existing fails of io-path.t
  (`.SPEC`/`.CWD` attributes) also fail on main = unrelated to this change. S16-io/S32-io/tmpdir/cwd/dir whitelist all green.
- **2026-06-23 (§D = making Cool scalar `.IO` coercion VM-native)**: follow-up to the IO::Path lexical slice (above).
  Drained the runner-up by measured spread, `.IO` (14 files: `"path".IO`/`$s.IO`/`42.IO`) = the fallback coercing Str/numeric to IO::Path.
  `.IO` is an arm of `dispatch_method_by_name` (`make_io_path_instance(target.to_string_value())` + null-byte check;
  IO::Path type objects are identity), which **reads `&self` = `$*SPEC`/`$*CWD` from env** = not pure, but the VM owns env,
  so a native read is possible. Added `try_native_io_coercion` to both non-mut/mut catch-alls (after the iterator construct, immediately before the final
  fallback), making **Str/Int/BigInt/Num/Rat/FatRat/Complex/Bool receivers + IO::Path(-subclass) type-object identity** native,
  **calling the same `make_io_path_instance`** = 1 operation = 1 implementation (interpreter dispatch untouched; same fn shared). Instance (user `.IO`/
  IO::Path/IO::Handle) / non-IO Package / aggregates / **Junction (autothread)** return `None` and stay on the interpreter = behavior-invariant
  (`("a"|"b").IO` returns a Junction; parity confirmed). Returns a new IO::Path, does not mutate the receiver = no writeback. Measured:
  fallback of `$s.IO`/`42.IO` → 0. pin `t/native-io-coercion.t`(18: literal/variable/numeric/type-object identity/null-byte
  dies/Junction autothread/`$*CWD` inheritance; PASSing on both raku and mutsu). S16-io/S32-io/tmpdir/cwd whitelist all green.
- **2026-06-23 (§D = making `IO::Path.absolute`/`.relative` VM-native)**: the cwd-dependent follow-up to the IO::Path lexical slice.
  `.absolute`/`.relative` derive a string by combining the path string with the **cwd** (`$*CWD` / the instance `cwd` attribute / process cwd), but
  the cwd is read via `&self`'s `resolve_path`/`get_cwd_path`/`apply_chroot`, which are **purely lexical (no FS stat)** = like `.IO`,
  the VM owns env/cwd so native dispatch is possible. Extracted into the new `&self` method `try_io_path_cwd_method` (keeping all branches for
  win32/cygwin/posix base arguments and the `$*CWD` fallback); `native_io_path` delegates right after `try_io_path_lexical` = **1 operation = 1 implementation**
  (deleted the `absolute`/`relative` arms). The VM calls it next to the IO::Path lexical dispatch (immediately before the `is_native_method` bounce, both non-mut/mut paths).
  `.resolve` (real FS canonicalize) returns `None` and stays in `native_io_path`. Returns a new string, receiver not mutated = no writeback, behavior-invariant.
  Measured: fallback of `$p.absolute`/`.relative` → 0. pin `t/native-io-path-cwd.t`(14: `$*CWD`/instance cwd/explicit base/variable receiver/
  round-trip; PASSing on both raku and mutsu). The 2 existing fails of io-path.t (`.SPEC`/`.CWD`) unchanged. S16-io/S32-io/tmpdir/cwd whitelist all green.
- **2026-06-23 (§D = making QuantHash / Map / Hash coercion VM-native for plain `Cool` scalar receivers too)**: residual sweep of the coercion
  drain. Re-measuring the method-fallback spread (whole whitelist, distinct file counts) confirmed **`Set`/`Bag`/`Mix`/`SetHash`/`BagHash`/
  `MixHash` still falling back in 8-11 files each**; identified the receivers = **plain scalar receivers** such as `"a".Set`/`42.Set`/`"blue".Set`
  (not list-like aggregates, so excluded by the VM gate's `list_like` check and dropping to the interpreter).
  The interpreter handles scalar `.Set` via `dispatch_to_set_with_what` = **`to_set` itself** (the Set/Bag/Mix arm has no Package special-casing even;
  scalars are single-element-ized by the `other =>` catch-all), so widening the native gate is **behavior-invariant with exactly the same implementation**.
  Consolidated the `list_like` judgment shared by `try_native_quanthash_coerce`/`try_native_map_hash_coerce` into the new helper
  `coerce_receiver_native_eligible`, adding **Str/Int/BigInt/Num/Rat/FatRat/Complex/Bool** to list-like aggregates
  (shared by both non-mut + mut catch-alls). Scalar `.Map`/`.Hash` (`42.Hash`) gives `X::Hash::Store::OddNumber` via the same `to_hash`/`to_map`
  = matches raku/interpreter (odd number). **Instance (`__baggy_data__`/Match captures/user coercion) / Package (type objects;
  `.Map`/`.Hash` return type objects) / Nil / Junction (autothread) return `None` and stay on the interpreter** = behavior unchanged. Returns new
  Set/Bag/Mix/Map/Hash values, does not mutate the receiver = no writeback. Measured: Set/Bag/Mix fallback in set.t/bag.t/mix.t
  11/10/10 → 2/2/2 (the remaining 2 are `__baggy_data__` Instance / type object = falling through by design). pin
  `t/native-scalar-quanthash-coerce.t`(39: literal/variable receivers × Set/Bag/Mix/SetHash/BagHash/MixHash/Map/Hash; PASSing on both raku and
  mutsu). set.t 248/248, mix.t 244/244, categorize 28/28, hash.t green. (bag.t 252 "Foo instance union" and classify.t 40
  "junction classify" are pre-existing per BLOCKERS.md [both non-whitelisted], unrelated to this change. The odd-number case of `(a=>1).Map`
  is a separate pre-existing bug of the Pair receiver [already inside the native gate], out of scope for this slice.)
- **2026-06-23 (§D = making `IO::Path`'s FS `stat`-only file tests / accessors VM-native)**: after the coercion drain was exhausted, the first step
  toward the **main prize** of ③ IO native methods. Of the FS-touching methods that the IO::Path lexical/cwd slices kept on the interpreter with `None`,
  made native the **group that completes with just a `stat` read** = the file tests `e`/`f`/`d`/`l`/`r`/`w`/`x`/`rw`/`rwx`/`z` and the stat accessors
  `mode`/`s`/`created`/`modified`/`accessed`/`changed` (spread measurement: `e`=15/`d`=11 files etc., spread via the temp-file helpers). These
  **acquire no `io_handles` at all, read no content, and emit nothing** = they just call `fs::metadata`/`exists` after resolving the path against the
  VM-owned cwd (`resolve_path`/`apply_chroot`/`get_cwd_path`, purely lexical). Extracted into the new `&self` method `try_io_path_fs_stat` (path_buf resolution) +
  static `io_path_stat_result` (from path_buf+p, stat + Failure shaping for all 16 arms); `native_io_path` delegates right after `try_io_path_cwd_method`
  = **1 operation = 1 implementation** (deleted the 16 arms from the match). The VM calls it next to the IO::Path lexical/cwd dispatch (immediately before the
  `is_native_method` bounce, both non-mut/mut paths). `slurp`/`lines`/`words`/`comb` (content reads, encoding flags) / `open`/`spurt` (`io_handles`
  acquisition, FS writes) return `None` and stay in `native_io_path` = next slice candidates. Returns new Bool/Int/Str/Failure, receiver not mutated = no writeback,
  behavior-invariant (same stat logic). Measured: fallback of `$p.{e,f,d,s,modified,…}` → 0. pin `t/native-io-path-fs-stat.t`(30,
  literal + variable receivers [mut path] × all file tests/accessors, missing-path Failure; PASSing on both raku and mutsu. `.modified`/`.accessed`/
  `.changed` avoid the separate pre-existing type difference mutsu=Int / raku=Instant by verifying with `> 0`/`.defined`). S16-io/S32-io/slurp/spurt whitelist
  all green (io-path.t's `.SPEC`/`.CWD` existing fails are non-whitelisted, unchanged). **Remaining IO native = content reads (slurp/lines) and handle
  acquisition (open/spurt) = the main prize directly touching ③'s `io_handles` ownership.**
- **2026-06-23 (§D = making `IO::Path`'s whole-file content reads `slurp`/`lines`/`words` VM-native)**: the content-read follow-up to the FS stat-only slice
  (above). `slurp` (spread 26 files)/`lines`(13)/`words` **read the whole file**, but acquire no `io_handles`
  and emit nothing = after resolving the path against the VM-owned cwd, just `fs::read[_to_string]` and split/decode the bytes. Flag parsing
  (`parse_io_flags_values`) and encoding lookup (`decode_with_encoding` = reading the VM-owned `encoding_registry`) are all `&self`
  reads, so native dispatch is possible. Extracted into the new `&self` gate `try_io_path_content_read` (returning `Option`) + the fallible body `io_path_content_read`
  (separated to use `?`). path_buf resolution became a 3rd site, so consolidated into the `resolve_io_path_buf` helper and delegated `try_io_path_fs_stat`
  to it too (dedup). `native_io_path` delegates to content right after the stat delegation = **1 operation = 1 implementation** (deleted the slurp/lines/words arms from the match).
  The VM calls it next to the stat dispatch (both non-mut/mut paths). **`comb` (regex/closure dispatch needs `&mut self`) / `open`/`spurt`
  (`io_handles` acquisition, FS writes) return `None` and stay in `native_io_path`** = the next slice (the ③ io_handles main prize). Returns new Str/Buf/Seq,
  receiver not mutated = no writeback, behavior-invariant (same read + split/decode; all branches for `:bin`/limit/`:!chomp`/non-utf-8 decode preserved).
  Measured: fallback of `$p.{slurp,lines,words}` → 0. pin `t/native-io-path-content-read.t`(24: literal + variable receivers [mut path] ×
  slurp/lines/words, `:bin` Blob, utf-8 decode, limit, `:!chomp`, missing-path dies; PASSing on both raku and mutsu). S16-io/S32-io/slurp/
  lines whitelist all green (io-path.t `.SPEC`/`.CWD` existing fails are non-whitelisted, unchanged). **Remaining IO native = `comb` (&mut) and handle-acquiring
  open/spurt = the main prize of ③'s `io_handles` ownership.**
- **2026-06-23 (§D = making `IO::Path`'s single-path FS mutations `spurt`/`mkdir`/`rmdir`/`unlink`/`chmod` VM-native)**: the **write-family follow-up** to the
  content-read slice (spurt spread 18 files). These 5 ops **mutate** the FS but acquire no `io_handles` at all =
  after resolving the path against the VM-owned cwd, they just make **a one-shot syscall** (`fs::write`/`create_dir_all`/`remove_dir`/`remove_file`/`set_permissions`)
  (`spurt` is open→write→immediate drop, leaving no handle). Encoding lookup (`encode_with_encoding` = reading the VM-owned registry)
  is `&self`. Extracted into the new `&self` gate `try_io_path_fs_mutate` (returning `Option`) + the fallible body `io_path_fs_mutate` (separated for `?`;
  `mkdir` round-trips an IO::Path via `class_name`). `native_io_path` delegates right after the content delegation = **1 operation = 1 implementation**
  (deleted the 5 arms from the match). The VM calls it next to the content dispatch (both non-mut/mut paths). **2-path ops (`copy`/`rename`/`move`/`symlink`/
  `link`; need destination path resolution) and handle-opening `open` (`io_handles` acquisition = `&mut self`) return `None` and stay in `native_io_path`** = the next
  ③ io_handles capstone. Returns new Bool/IO::Path/Failure, receiver not mutated = no writeback, behavior-invariant (same syscall +
  all branches for `:append`/`:createonly`/`:enc`+BOM/Buf preserved). Measured: fallback of `$p.{spurt,mkdir,rmdir,unlink,chmod}` → 0. pin
  `t/native-io-path-fs-mutate.t`(22: literal + variable receivers [mut path] × 5 ops, `:append`/`:createonly` Failure, Buf spurt, mkdir
  recursive, non-empty rmdir Failure, chmod mode; PASSing on both raku and mutsu). S16-io/S32-io/spurt/dir whitelist all green. (`.unlink` of missing
  is a separate pre-existing difference mutsu=False/raku=True [the pre-move interpreter also did `Err(NotFound)=>Bool(false)`], out of scope for this slice = not pinned.)
  **Remaining IO native = `comb` (&mut) and `open` (`io_handles` acquisition) = the capstone of ③'s `io_handles` ownership.**
- **2026-06-23 (§D ③ capstone = making `IO::Path.open` VM-native = the VM acquires `io_handles` directly)**: the **symbolic milestone** of ③ (state ownership).
  `open` is the only IO::Path FS method that **mutates the `io_handles` table** (`open_file_handle`→
  `insert_handle_state` acquires a handle id = `&mut self`). The ledger's original rationale for "native-method (IO family) is ③-blocked" was
  exactly this = `io_handles` being interpreter-owned state. But since #2760-2772, `io_handles` is **an `Arc<RwLock>` shared
  field owned by the VM** = `open_file_handle` can be called directly from the unified struct's `self`. Extracted the new `&mut self` gate `try_io_path_open`
  (`resolve_io_path_buf`(&self)/`parse_io_flags_values`(&self) return owned values, so no borrow conflict with the subsequent `&mut self` `open_file_handle`);
  `native_io_path` delegates right after the fs_mutate delegation = **1 operation = 1 implementation** (deleted the open arm from the match). Since **both of the VM's
  non-mut/mut dispatch functions are `&mut self`** (`try_compiled_method_or_interpret_inner`/
  `try_compiled_method_mut_or_interpret`), both `"x".IO.open` (non-mut) and `$p.open` (mut) are native. All flags `:r`/`:w`/`:a`/`:rw`/`:bin`/
  `:enc`/`:create`/`:exclusive` and the Failure-on-error shaping are preserved = behavior-invariant (same `open_file_handle`). Measured:
  fallback of `$p.open` → 0. **∴ The whole open→read/write→close lifecycle runs VM-native without a catch-all bounce**
  (the handle methods get/lines/print/say/close/tell/eof/seek/flush etc. are already native). pin `t/native-io-path-open.t`(20: literal +
  variable receivers [mut path] × :r/:w/:a/:exclusive/:bin, missing/directory Failure, opened state; PASSing on both raku and mutsu). S16-io/S32-io/
  open whitelist all green (io-handle.t's `.say` existing fail also fails on main = non-whitelisted, unrelated to this change). **Remaining IO native = `comb`
  (regex/closure dispatch is `&mut self`) and the 2-path ops (copy/rename/move/symlink/link) only = ③'s IO native is nearly complete.**
- **2026-06-23 (§D = making `IO::Path`'s 2-path FS ops `copy`/`rename`/`move`/`symlink`/`link` VM-native = completing the IO::Path FS family)**:
  follow-up to the open capstone = **the last non-native members** of the IO::Path FS method family. These 5 ops resolve **both the receiver path and
  the destination/link-name path** against the VM-owned cwd (`resolve_path`/`resolve_io_path_buf`, `&self`), then just make a one-shot syscall (`fs::copy`/`fs::rename`/`unix_fs::symlink`/
  `fs::hard_link`) = no `io_handles` needed, all `&self`. Extracted into the new `&self` gate `try_io_path_two_path_op` (returning `Option`) + the fallible
  body `io_path_two_path_op` (separated for `?`). `native_io_path` delegates right after the open delegation = **1 operation = 1 implementation** (deleted the copy/rename|move/
  symlink/link arms from the match; the `dir`/`watch` in between kept). The VM calls it next to the open dispatch (both non-mut/mut paths). Same-file/createonly
  checks, `X::IO::Copy`/`Rename`/`Move` Failure shaping, `:absolute` symlink, and all hard-link branches preserved = behavior-invariant. Measured:
  fallback of `$p.{copy,rename,move,symlink,link}` → 0. pin `t/native-io-path-two-path.t`(18: literal + variable receivers [mut path] ×
  5 ops, copy-onto-self/createonly Failure, symlink resolve, hard link; PASSing on both raku and mutsu. The dir is cleanly regenerated each run to
  prevent stale file contamination). S16-io/S32-io/copy/rename whitelist all green. **∴ The IO::Path FS method family (stat/content-read/fs-mutate/open/2-path)
  is now fully VM-native = §D ③'s IO native is nearly complete. Only `comb` remains (regex/closure dispatch is `&mut self`; a separate axis).**
- **2026-06-23 (§D = making `.encode`/`.decode` (Str↔Buf conversion) VM-native)**: the next clean native-method drain after the IO::Path family
  (spread: `encode`=16 files). **The explicit-encoding forms** (`"x".encode("utf-16")`/`$buf.decode("ascii")`) bounced to the interpreter at the
  `dispatch_method_by_name` catch-all (0-arg `.encode` is already native in `methods_0arg`). These are pure Str↔Buf conversions reading the VM-owned
  `encoding_registry` (`find_encoding`/`encode_with_encoding`/`decode_with_encoding` = all `&self`), no `io_handles` needed. Placed the new
  `pub(crate)` helper `try_native_encode_decode` **on the `crate::runtime` side (methods_io_dispatch.rs)** (`dispatch_encode`/
  `dispatch_decode` are `pub(super)` so it is called from the same crate::runtime; same pattern as IO::Path), called from the VM's non-mut/mut catch-alls
  (right after `try_native_io_coercion`, immediately before the final bounce) = **1 operation = 1 implementation**. **Conservative gate**: `.encode` only for plain Cool scalars (Str/Int/BigInt/Num/Rat/
  FatRat/Complex/Bool); `.decode` is gated by `dispatch_decode` itself to Buf/Blob Instances (otherwise `None`). User Instances (custom
  `.encode`/`.decode` resolve earlier as compiled methods), Supply (its own chunk-encode; the interpreter arm excludes it), and `.encode` on Buf receivers
  fall through = behavior-invariant. Returns new Buf/Str, receiver not mutated = no writeback. Measured: fallback of `"x".encode("utf-16")`/`$buf.decode` → 0.
  pin `t/native-encode-decode.t`(23: literal + variable receivers [mut path] × encode utf-8/utf-16/ascii/latin-1, Int/Bool/Rat
  encode, Buf/Blob decode, round-trip; PASSing on both raku and mutsu. Including the caveat that utf-16 `.elems` = the number of 16-bit units). encoding/buf/S32-str/encode
  whitelist all green.
- **2026-06-23 (§D = making `IO::Path.comb` VM-native = 100% completion of the IO::Path FS family + fixing the no-arg comb bug)**: **the last method** of the
  IO::Path FS method family. `comb` reads the whole file and combs the content, but the matcher dispatch (`dispatch_comb_with_args`) needs `&mut self`
  because regex/closure matchers run the match engine — yet `io_handles` is untouched. Extracted into the new `&mut self` helper
  `try_io_path_comb` (read → error shaping via `match` for `?`); `native_io_path` delegates right after the two-path delegation = **1 operation = 1 implementation**
  (deleted the comb arm from the match). The VM calls it next to the two-path dispatch (both non-mut/mut paths). **Also fixed a pre-existing bug at the same time**: argumentless
  `$path.IO.comb` (no matcher) returned an empty Seq (the old arm mapped `dispatch_comb_with_args`'s `None` to empty) → now splits the content into
  graphemes (matching no-arg `Str.comb` / Rakudo). The `None`-no-arg semantics of `dispatch_comb_with_args` are shared with the generic caller
  (methods_dispatch_match.rs:187), so **the fix was kept local to the IO::Path comb helper** (None→graphemes). Measured:
  fallback of `$p.comb`/`$p.comb(/re/)`/`$p.comb(N)` → 0. pin `t/native-io-path-comb.t`(17: literal + variable receivers [mut path] × no-arg
  graphemes, regex, Int chunk, Str fixed, limit, empty file, unicode; PASSing on both raku and mutsu). S16-io/S32-io/comb whitelist all green.
  **★ ∴ The IO::Path FS method family (stat/content-read/fs-mutate/open/2-path/comb) is 100% VM-native = §D ③'s IO native is complete.**
  **The clean pure-value native-method drain is also exhausted** (remaining categories = ③ built-in type ctors [Buf.new etc.] / MOP carrier [WHAT/name/can] /
  landmines [Instance.Str/.Stringy/.raku/.gist] / block-exec slow path [map/grep] / concurrency [Supply/tap] / typed-array mutators —
  all premised on a different axis or a structural blocker). The next §D is either ③ built-in type ctors or the substrate for deleting the tree-walk dispatch chain (needs design; large).
- **2026-06-23 (§D ③ = making `.new` of `::`-namespaced classes / built-in exception types VM-native)**: first slice of the ③ built-in ctor fork.
  The `cn_resolved.contains("::")` guard in `is_native_default_constructible` excluded **all `::`-namespaced classes** (user `A::B` /
  built-in exception types `X::AdHoc`, `X::TypeCheck::Binding` …) from native construction = the only blocker. Kept the `[` (parametric) guard and
  deleted the `::` guard. Furthermore, built-in exception types are registered in the registry with `attributes: Vec::new()` (zero declared attributes;
  named args become an attribute bag via `is_attribute_buildable`'s undeclared-name `true` fallback), so `has_attribute` was false → excluded by the tail return. Changed to
  `has_attribute || is_exception`, allowing native if the MRO contains `Exception` (`build_native_default_instance` stores named args via the same
  `is_attribute_buildable` = byte-identical with the interpreter). **Added `materialize_exception_message_in_result` at the VM call site
  (vm_call_method_compiled.rs:127)** = equivalent to the interpreter slow path (methods.rs:3369 calls it after `dispatch_new`):
  runs the user `message` method once at construction and caches it into the `message` attribute (no-op for built-in exceptions / non-exceptions). When running a user `message`,
  `method_dispatch_pure=false` (preserving caller slot reconcile). Widened the visibility of `materialize_…` from `pub(super)`→`pub(crate)`. Measured: the `new` fallback of `X::AdHoc.new`/
  `X::TypeCheck::Binding.new`/`A::B.new`/`P::Q::R.new` → 0. Confirmed via stash comparison that native==interpreter output matches exactly
  (the raku differences on `message`/no-arg are the F-track `.message` unimplemented gap that predates this change, unrelated). pin
  `t/native-namespaced-and-exception-ctor.t`(16). make test 10971; S04/S12/S32 construction whitelist all green. **Remaining ③ ctor candidates =
  concurrency types like Promise, misc built-in types (those with special construction that `is_attribute_buildable` cannot attribute-bag-ify).**
- **2026-06-23 (§D ③ = making `.new` of the `Lock` family (`Lock`/`Lock::Async`/`Lock::Soft`) VM-native)**: 2nd slice of the ③ ctor fork.
  Lock construction is pure data (a process-global counter bump via `next_lock_id()` + the `async` flag of `Lock::Async`; no env/registry/user code needed).
  Added a Lock arm to the static `try_native_builtin_construct` (called by the VM call site vm_call_method_compiled.rs:145 with `method_dispatch_pure=true`);
  the interpreter dispatch_new's existing arm (inside `match base_class_name`) is left in place with only a comment added, following the
  "Shared with the VM's native fast path" duplicated-implementation convention like `Slip`/`IterationBuffer` (4 lines, byte-identical). Measured: the `new`
  fallback of `Lock.new`/`Lock::Async.new`/`Lock::Soft.new` → 0. Confirmed native==interpreter via stash comparison. pin `t/native-lock-ctor.t`(9). make test 10964. **★ A known separate-axis bug (unrelated to this change;
  reproduces on main too): the captured-outer `$n` writeback drop in `$lock.protect({ $n++ })` (`.protect`'s closure capture writeback; nothing to do with the ctor).**
  **Remaining ③ ctor candidates = Promise/Channel (SharedPromise/channel state), the QuantHash family (Bag/Set/Mix/SetHash; element counting), Capture, misc.**
- **2026-06-23 (§D ③ = making `.new` of `Promise`/`Channel`/`Supplier`/`Supplier::Preserving` VM-native)**: 3rd slice of the ③ ctor fork
  = simple concurrency primitives. `Promise.new`=`Value::Promise(SharedPromise::new())` (an empty planned promise; pure shared state),
  `Channel.new`=`Value::Channel(SharedChannel::new())` (empty channel), `Supplier`/`Supplier::Preserving`=`{emitted:[], done:False,
  supplier_id:next_supplier_id()}` (emission log + process-global id) = none need env/registry/user code. Added 3 arms to the static
  `try_native_builtin_construct`; the interpreter dispatch_new's existing arms are left with comments only (the byte-identical duplicated-implementation convention).
  Measured: the `new` fallback of all 4 kinds → 0. `$p.keep(42)`/`$c.send.list`/`$s.Supply.tap` functionality matches raku. pin `t/native-concurrency-ctor.t`(10).
  make test 10984. **★ A known separate-axis gap (unrelated to this change): late-tap replay of pre-tap emits for `Supplier::Preserving` is unimplemented (mutsu does not
  buffer/replay; nothing to do with construction).** **Remaining ③ ctor candidates = the QuantHash family (Bag/Set/Mix/SetHash; element counting = needs `&mut self` type
  checks, not static), Capture, Proxy (FETCH/STORE closures), misc.** **★ perf note = calling `pub(crate)` ctors like `SharedPromise::new()`
  directly from the VM static path = one hop shorter than delegating to the interpreter.**
- **2026-06-24 (§D ③ = making `.new` of the QuantHash family (`Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/`MixHash`) VM-native)**: 4th slice of the ③ ctor
  fork. Unlike the previous 3, QuantHash construction is **`&mut self`** (element counting + parameterized type check `type_matches_value` + container metadata
  `tag_container_metadata`), so it doesn't fit the static `try_native_builtin_construct` = **the 3 arms of dispatch_new (414 lines) were wholesale extracted into
  the `&mut self` helper `try_native_quanthash_construct` of the new module `methods_quanthash_ctor.rs`** (same pattern as the IO::Path family; "1 operation =
  1 implementation"). The 3 dispatch_new arms were replaced with a delegation to the helper (`return self.try_native_quanthash_construct(*class_name, base_class_name,
  &type_args, args)`) = **a byte-identical wholesale move with no copy** (unlike the previous 3 slices' "comment-left duplicated implementation", a true single impl). The VM
  call site adds the new wrapper `try_native_quanthash_construct_for_package(&[Value])` (parametric name strip → `is_quanthash_ctor_type` gate → clone args only on
  match) to both non-mut/mut paths (the parametric `Set[Int].new` also resolves the base via `parse_parametric_type_name`). env-pure
  (only value construction + container metadata tagging) = `method_dispatch_pure=true`. Visibility: widened `strip_named_pair_args` to `pub(super)` (the other
  `value_to_list`/`is_lazy_*`/`type_matches_value`/`tag_container_metadata`/`parse_parametric_type_name` are already pub(super)/pub(crate)).
  Measured: the `new` fallback of all 6 kinds → 0; parametric (`Bag[Int]`/`Set[Int]`) and type-check failure (`X::TypeCheck::Binding`) also match raku. pin
  `t/native-quanthash-ctor.t`(18). make test 11018; setbagmix/baggy/mix whitelist all green. **★ Remaining ③ ctor candidates = `Capture.new` (unimplemented in mutsu =
  needs separate implementation), `Proxy` (FETCH/STORE closures), low-traffic static candidates like `Match`/`FakeScheduler`, `Array`/`Hash` (more complex due to shaped/container
  metadata). With the clean big fish QuantHash consumed, the remainder is small fry or unimplemented features.**
- **Deferred (2026-06-23): generic `Instance.Str`/`.Stringy` coercion**. Runner-up by spread (`Stringy`=22/`Str`=11 files), but the Instance.Str reaching the VM
  catch-all is **not limited to generic objects (`to_string_value()`)** — it includes special stringification of built-in types (`Buf.Str`→
  `X::Buf::AsStr` throw, `Attribute/BOOTSTRAPATTR.Str`→name, `has $.Str`'s public accessor→attribute value). These are not gated by `is_native_method`
  and get interpreter special-handling before arm 1499, so nativizing with `to_string_value()` breaks them (regressions confirmed in say.t/buf.t/
  attributes.t). Even adding `has_user_method`/`has_public_accessor`/built-in type exclusions, landmines kept surfacing one after another = not enumerable and
  cannot be safely separated, so deferred. The clean §D coercion results are settled with the IO::Path family (lexical/`.IO`/absolute-relative).
- **2026-06-24 (§D ③ = making `.new` of `FakeScheduler`/`Proxy`/`Match` VM-native)**: the clean static leftover sweep of the ③ ctor fork. All 3 are
  pure data assembly (`FakeScheduler` is a process-global counter + virtual-time 0.0 seed; `Proxy` wraps evaluated FETCH/STORE callables;
  `Match` slices `orig[from..to]` and stores positional/named captures as attributes) = no env/registry/user code needed. Extracted each construction into per-type
  helpers (`build_native_fakescheduler_value`/`build_native_proxy_value`/`build_native_match_value`), with **both** `try_native_builtin_construct`
  and the interpreter's `dispatch_new` arm **calling the same helper** (true single impl, byte-identical). Measured: the `new` fallback of all 3 kinds → 0.
  pin `t/native-misc-ctor.t`(14; Proxy/Match verified for raku parity).
- **2026-06-24 (§D ③ = implementing `Capture.new`)**: `Capture` had no constructor at all and `Capture.new` was an error. raku semantics
  (verified) = default `Capture.new` is an **empty Capture**. Named args are dropped (Capture has no buildable public attributes and `bless` ignores them); positional
  args are rejected (named-only `Mu.new` = "Default constructor for 'Capture' only takes named arguments"). Populated Captures are made with `\(...)`.
  Named args are `Value::Pair`; everything else (literals, positional `"a" => 1` `ValuePair`) dies as positional. Pure data =
  `try_native_builtin_construct`. Known unchanged gap (not a regression) = `.new` on a Capture **instance** receiver (`\(1).new`) is still an error = a separate,
  broader gap of instance `.new` delegation for built-in value variants. pin `t/native-capture-ctor.t`(9; PASSes on raku too).
- **2026-06-24 (§D ③ = making `.new` of `Array`/`List`/`Positional`/`array`/`Hash`/`Map` VM-native)**: draining the aggregate ctors. Unlike the pure-data
  statics, `&mut self` (shaped-dim parsing `:shape(...)` + `:data`/positional population + X::Assignment shape errors, Slip/Range/Seq flattening,
  parameterized type check `Array[Int].new`/`Hash[Int].new`→`X::TypeCheck::Assignment`, container metadata tagging). Like QuantHash, the 2 arms (~310 lines) were
  **wholesale extracted** into the new module `methods_aggregate_ctor.rs` (`try_native_array_construct`/`try_native_hash_construct`), the interpreter's
  `dispatch_new` arms are delegations, and the VM calls via `try_native_aggregate_construct_for_package` (parametric name → base+type_args resolution) = true single
  impl, byte-identical. Measured: the `new` fallback of all 6 kinds → 0. pin `t/native-aggregate-ctor.t`(16; raku parity subset. mutsu-specific behavior of
  shaped/Hash-named-data is covered by roast S02-types/array.t, hash.t, S09-typed-arrays/*). **★ This exhausts the clean nativizable candidates among
  `dispatch_new`'s built-in type ctors. The remaining arms are state/FS/process-dependent (IO::Socket::INET [socket], Distribution/CompUnit::Repository [FS], Proc::Async
  [process], Backtrace [call stack], Seq [predictive iterator carrier]) or error-only (HyperWhatever/Whatever/Instant).**
- **2026-06-24 (§D ③ = making `.new` of the allomorphs (`IntStr`/`NumStr`/`RatStr`/`ComplexStr`) + `ObjAt`/`ValueObjAt` VM-native)**: re-measuring the ③ ctor fork
  (temporary probe of `new`-fallback receivers over the whole whitelist) revealed the most frequent receivers remaining after aggregate are
  **in `dispatch_new_and_constructors` (slow-path method dispatch), not `dispatch_new`**: `RatStr`(892)/`IntStr`(306)/`ComplexStr`(176)/`NumStr`(125)
  = allomorphs, `ObjAt`/`ValueObjAt` (18 total). These are fully **pure-static** (args→instance; no `&self` at all = touching no registry/env/FS either).
  Allomorph `.new(numeric, string)` just makes the inner numeric (unwrapping if the arg is an allomorphic `Mixin`) into a `Value::mixin` with a `Str` override; `ObjAt`/
  `ValueObjAt` `.new(which)` just stores the stringification of the first positional into the `WHICH` attribute. **Added 2 arms to the static `try_native_builtin_construct`
  (called by VM call sites vm_call_method_compiled.rs:160/1788 with `method_dispatch_pure=true`)**, with the new static helpers `build_native_allomorph_value`/
  `build_native_objat_value` called by both it and `dispatch_new_and_constructors` (inline implementation removed) = **1 operation = 1 implementation** (same pattern as FakeScheduler/Proxy/Match).
  The arity error wording is mutsu-specific ("requires two arguments" / "Too few positionals" = a pre-existing difference from raku) and preserved during extraction =
  byte-identical. Measured: the `new` fallback of `IntStr.new`/`RatStr.new`/`ObjAt.new` etc. → 0 (val.t's `new` vanished from the histogram). pin
  `t/native-allomorph-objat-ctor.t`(23; PASSing on both raku and mutsu). S32-str/val.t, S03-operators/set_union.t, S32-num/rounders.t, rat.t, S02-types/num.t
  whitelist all green. (The existing fails of allomorphic.t 107/113 are identical on main = a separate-axis gap in `.ACCEPTS`/`.Numeric`, unrelated to this change.) **Remaining `new` fallback receivers
  = Proc::Async [process] / Failure [reads `$!` env] / IO::Socket::INET [socket] / IO::Path family [registry; separate slice candidate] / CallFrame [call stack] /
  Seq [iterator carrier] = all state-dependent or a different axis.**
- **2026-06-24 (§D ③ = making `.new` of the `IO::Path` family (`IO::Path`/`::Unix`/`::Win32`/`::Cygwin`/`::QNX`) VM-native = the IO::Path ctor capstone)**:
  closing the IO::Path nativization theme = the method family (stat/content-read/fs-mutate/open/2-path/comb, #3499–3511) is 100% native, but **only the ctor
  still bounced through the catch-all** (probe measured `IO::Path::Win32`/`::Cygwin`/`::Unix`/`IO::Path` ~112 occ total). `.new` is **pure path-string assembly**
  = it joins the positional path / an IO::Path instance (reusing `path`) / the basename+dirname+volume triple with the SPEC-derived separator and attaches
  CWD/SPEC attributes. The only `&self` dependency is a **registry read** (`class_mro` = detecting an IO::Path-instance argument) = VM-owned (phase ②) = no FS/cwd/env/user code.
  The IO::Path arm of `dispatch_new` (~117 lines) was **wholesale extracted** into the new `&mut self` helper `build_io_path_instance` (the interpreter arm is a
  delegation; the one-time registry registration of SPEC-variant subclasses is kept). The VM gate `try_native_io_path_construct` is restricted to **the built-in IO::Path family only**
  (`is_io_path_lexical_class` = the same gate as the lexical method slice = does not intrude on user subclasses' custom new), called from both non-mut/mut call sites
  (right after the aggregate ctor arm; `method_dispatch_pure=true`) = **1 operation = 1 implementation**, byte-identical. Measured: the `new` fallback of `IO::Path.new`/`IO::Path::Win32.new` etc. → 0. pin `t/native-io-path-ctor.t`(19;
  positional/triple/instance-arg/Win32-Unix-Cygwin subclasses/CWD/empty-null dies; PASSing on both raku and mutsu). S32-io/S16-io/tmpdir/cwd/S11-compunit whitelist
  all green (io-path.t 34/35's `.SPEC`/`.CWD` attribute existing fail is identical on main = non-whitelisted, unrelated to this change). **Remaining `new` fallback = Proc::Async/Failure
  [`$!` env]/IO::Socket::INET/CallFrame/Seq = state-dependent or a different axis.**
- **2026-06-24 (§D ③ = making `Failure.new($exception?)` VM-native)**: the **highest count** among the remaining `new` fallbacks (probe measured 2593; driven by `fail`/explicit construction).
  `Failure.new` is **pure data assembly reading only VM-owned state** = it takes the explicit exception argument (else `$!` from env, else the default `X::AdHoc("Failed")`),
  wraps it in `X::AdHoc` unless it's already `Exception`/`X::`/`CX::` (MRO read `mro_readonly`), and builds a `{exception, handled:false}` instance.
  The `&self` dependencies are an **env read (`$!`) + a registry read (`mro_readonly`)** = VM-owned (after single-store-ification, `self.env` is identical for VM/interpreter) = no FS/process/socket/
  user code. The Failure arm of `dispatch_new_and_constructors` (~52 lines) was **wholesale extracted** into the new `&self` helper `build_native_failure_value` (the interpreter arm is a
  delegation). The VM calls it from both non-mut/mut catch dispatches (right after the IO::Path ctor arm; `class_name=="Failure"` gate; `method_dispatch_pure=true`) = **1 operation =
  1 implementation**, byte-identical. Measured: the `new` fallback of `Failure.new` (explicit/string-wrap/`$!`/argless, all paths) → 0. pin `t/native-failure-ctor.t`(11; PASSing on both raku and
  mutsu. `X::AdHoc` is constructed with `:payload` = avoiding the pre-existing detail that `.message` reads the payload). S04-exceptions/S04-statement-modifiers/
  S05-capture/named/S06-advanced whitelist all green. **Next slice candidates among the remaining `new` fallbacks (2026-06-24 measured correction — correcting the "state-dependent" classification)**:
  ① **`Proc::Async.new` = actually fully pure data** (process spawn is in `.start`; the ctor is just arg parsing + the `next_supply_id()` free fn + `SharedPromise::new()` +
  Supply attribute construction; zero `&self` dependency) = wire a `build_native_proc_async_value` static helper into `try_native_builtin_construct` (same shape as Promise/Channel; easiest).
  ② **`IO::Socket::INET.new` = io_handles-dependent but the same shape as `IO::Path.open` (#3507)** (`dispatch_socket_inet_new` acquires a VM-owned io_handle via `insert_handle_state`)
  = delegate to the existing helper via `try_native_socket_inet_construct`. **Truly structurally blocked (ctor fork completes after ② lands)**: `CallFrame` [call stack carrier] = a different axis.
  (`Seq.new` was made native in #3533 = below.) **∴ ctor-fork remainder = only 2 items: Proc::Async (pure) + IO::Socket::INET (io_handles).**
- **2026-06-24 (§D ③ = making `Seq.new($iterator?)` VM-native, #3533)**: re-evaluated `Seq.new`, which the previous entry conservatively classified as "iterator carrier =
  a different axis (impure)", and made it native. **The carrier state itself is VM-owned** (the `predictive_seq_iters` field + env's `__mutsu_predictive_seq_iter::` internal keys +
  the global deferred-iter side table keyed by the Seq's Arc) = after single-store-ification `self` is identical for VM/interpreter, so the native gate can perform the same writes. **Construction does not
  eagerly pull** (a PredictiveIterator is stashed in the carrier; materialized `items`/`stuff` instances are element copies; other iterators get deferred registration; no-arg is a
  pre-consumed Seq) = iterator consumption happens later at consumption time, separate from construction = no FS/process/user code needed. The Seq arm of `dispatch_new` (~46 lines) was **wholesale extracted**
  into the new `&mut self` helper `try_native_seq_construct` (the interpreter arm is a delegation). The VM calls it from both non-mut/mut catch dispatches (right after the Failure ctor arm;
  `class_name=="Seq"` gate; `method_dispatch_pure=true`) = **1 operation = 1 implementation**, byte-identical. User subclasses (`class S is Seq`) resolve class_name to
  their own name and miss the gate, staying on the interpreter. pin `t/native-seq-ctor.t`(11; PASSing on both raku and mutsu. The `.raku` `$(...)` itemization difference is pre-existing,
  unrelated). S32-list/seq.t(50)/skip.t/tail.t/rotor.t, S07-iterators/range-iterator.t(103) all green. **Remaining `new` fallback = Proc::Async (pure; next) /
  IO::Socket::INET (io_handles; next) / CallFrame [call stack carrier, a different axis].**
- **2026-06-24 (§D ③ = making `Proc::Async.new(@cmd, :w, :enc)` VM-native, #3535)**: re-evaluated `Proc::Async.new`, conservatively classified as "state-dependent"
  two entries earlier, and made it native. **The ctor is fully pure data** = arg parsing (positional command + `:w`/`:enc` flags) + 3
  process-global supply ids (`next_supply_id` = a bare global counter) + construction of empty stdout/stderr/merged `Supply` instances only. **Actual process spawn is
  on the `.start` side** = at ctor time env/registry/io_handles/user code are entirely untouched (zero `&self` dependency). The Proc::Async arm of `dispatch_new` (~66 lines) was
  **wholesale extracted** into the new static helper `build_native_proc_async_value(class_name, args)` (the interpreter arm is a delegation), and the VM wires a
  `cn=="Proc::Async"` arm into the existing `try_native_builtin_construct` in the same shape as `Promise`/`Channel`/`Supplier` = **1 operation = 1 implementation**, byte-identical.
  pin `t/native-proc-async-ctor.t`(8; PASSing on both raku and mutsu. Includes a real echo/cat round-trip + exit code + `:w` stdin).
  t/proc-async.t(23), roast/S17-procasync/basic.t(47)/print.t(16) all green.
  **Remaining `new` fallback = IO::Socket::INET (io_handles; next; same shape as `IO::Path.open`) / CallFrame [call stack carrier, a different axis].**
- **2026-06-24 (§D ③ = making `IO::Socket::INET.new(...)` VM-native, #3536)**: the clean finale of the ③ ctor fork. The ctor of `IO::Socket::INET.new` (both `:listen`
  server and client modes) performs the real bind/connect, but **the write target is the VM-owned `io_handles`** (`insert_handle_state` = the same shape as the already-native
  `IO::Path.open` [#3507]) = env/registry/user code untouched. Widened the existing `&mut self` helper `dispatch_socket_inet_new` (the single impl that the interpreter's `dispatch_new`
  arm called) from `pub(in crate::runtime)`→`pub(crate)`, and the VM **calls the same helper directly** from both non-mut/mut catch dispatches (right after the Seq ctor arm;
  `class_name=="IO::Socket::INET"` gate; `method_dispatch_pure=true`) = **1 operation = 1 implementation**, byte-identical (no new copy).
  User subclasses resolve class_name to their own name and miss the gate, staying on the interpreter. pin `t/native-socket-inet-ctor.t`(7; PASSing on both raku and mutsu. Real loopback
  client/server round-trip + invalid port/family dies + independent listener). t/socket.t(3), roast/S32-io/IO-Socket-INET.t(32)/IO-Socket-INET-UNIX.t(8)/
  socket-accept-and-working-threads.t(15)/socket-fail-invalid-values.t(4)/socket-host-port-split.t(2) all green. **∴ The ③ ctor fork is complete** = every pure-value /
  VM-owned-state built-in ctor is native. **Remaining `new` fallback = CallFrame [call stack carrier], error-only (HyperWhatever/Whatever/Instant) =
  a different axis or structurally blocked.** Next is the §D main prize = (b) the tree-walk dispatch-chain removal substrate or VM-ification of multi-dispatch.
- **2026-06-24 (§D multi-dispatch = VM-ification of proto sub dispatch (trivial body), #3541)**: started on the next §D main prize after ③ ctor completion = VM-ifying multi-dispatch.
  Measurement (S06 sample, `MUTSU_VM_STATS`) showed **bare multis have 0 fallback** (OTF'd in PR-4) but **proto multis fall back 100%** (2 layers: the proto sub call itself
  + the internal `__PROTO_DISPATCH__`; each candidate body tree-walks via `call_proto_dispatch`→`run_block`). **Trivial-body protos (`proto foo {*}` / bodyless) are
  dispatched directly at the VM call site**: added a proto fast-path at the top of the else block of `dispatch_func_call_inner`; the new helper `vm_resolve_trivial_proto_candidate`
  ① excludes interpreter-handled names ② fetches the proto def via `resolve_proto_function` and verifies the body is trivial (empty or `[Expr(Whatever)]` after excluding the `SetLine` marker)
  ③ **gates on the proto's own sig via `method_args_match`** (`proto f(Int) {*}` rejects a Str — fixing the regression caught by S06-multi/proto.t subtest 26) ④ resolves the winner candidate via `resolve_proto_candidate_with_types`
  (VM-owned registry = phase ②) ⑤ returns it if OTF-compilable and non-state. If returned, `compile_and_call_function_def` **executes the candidate body compiled**
  (fully bypassing the tree-walk proto body + the `__PROTO_DISPATCH__` round-trip + the candidate `run_block`). `nextsame`/`samewith`/`callwith` work via the same function's `push_multi_dispatch_frame` +
  samewith context (the mechanism verified in PR-4). Non-trivial bodies / non-OTF candidates / where candidates / sig mismatches return `None`, keeping the conventional interpreter fallback = byte-identical.
  Visibility: widened `resolve_proto_candidate_with_types`/`resolve_proto_function`/`method_args_match` to `pub(crate)`. Measured: fallback of `proto factorial` 100%→0%,
  `__PROTO_DISPATCH__` in the S06 sample 50→30 (the remaining 30 = where candidates / non-trivial bodies / non-OTF candidates, correctly falling back). pin `t/proto-vm-dispatch.t`(12; PASSing on both raku and mutsu =
  recursion / per-type candidates / samewith / nextsame / proto-sig gate [EVAL-wrapped] / non-trivial body guard). All 87 whitelisted S06 files + roast/S06-multi/proto.t(27) green, make test 11202.
  **Remaining multi-dispatch fallbacks**: OTF-ification of where-constrained candidates (the candidate-side `def_is_otf_compilable` excludes where) / VM-ification of non-trivial proto bodies / `@_` slurpy recursive
  plain subs (a separate category). **★ Lesson: a `{*}` proto body gets a `SetLine` marker prepended at registration, making body.len=2 = the triviality check must exclude the marker. The proto's own sig is a
  gate independent of the candidates' sigs = when bypassing, always validate with `method_args_match` (otherwise the type check of `proto f(Int)` is skipped).**
- **2026-06-25 (§D = OTF-ification of imported `is test-assertion` subs)**: removed the `!def.is_test_assertion` exclusion from `def_is_otf_compilable_module_single`
  (`call_compiled_function_named` pushes the test-assertion line context, so even when OTF-compiled, the caller-line reporting of assertion failures is identical to the interpreter).
  **But removing the exclusion alone had zero effect**: Test::Assuming/Test::Util's `is-primed-sig` etc. are parsed via `Expr::Call` as imported functions, and kept being rejected by the OTF gate
  due to the `Capture |cap` sub_signature. **The key = re-registering imported `is test-assertion` subs into the using scope** (new `InlineModuleExport.is_test_assertion`
  field, propagated from SubDecl; the fallback regex also detects `is test-assertion`), putting them on the same parse path as locally declared assertion helpers (`known_call_stmt`→`Stmt::Call`)
  = reaching OTF-compilable dispatch. Measured: the S06-currying function-fallback 195 (is-primed-sig=171/is-primed-call=21/priming-fails-bind-ok=3)→2.
  `priming-fails-bind-ok` (the body's `try`/`CATCH`) correctly remains on the conservative gate, byte-identical. pin `t/test-assertion-module-otf.t`(7). make test 11471 PASS.
  **★ A separate bug split off (not included in this PR; needs separate work) = `use`'s export scan (`extract_exported_names`→`parse_program_partial`→`set_original_source(module_src)`)
  overwrites the parser's `ORIGINAL_SOURCE` with the module's temporary String (dropped immediately) → dangling → the using file's `current_line_number` collapses to all 1s — a general bug.**
  Fixing this with `snapshot_source_state`/`restore_source_state` makes caller lines match raku, but **the line-number normalization exposes existing latent bugs that "accidentally PASSed
  on the clobber-derived line 1"** (measured 2: S04-phasers/enter-leave.t#28 [the ENTER-rvalue of `sub f(){ ENTER 'X' }` is Nil] and keep-undo.t#10 [UNDO]. Standalone repros also give
  Nil = genuine phaser bugs independent of line numbers, but the tests only PASSed on the clobbered line 1). Blast radius unknown (calibration-by-clobber may lurk throughout roast), so
  the ORIGINAL_SOURCE fix + the exposed phaser bug group are split off from this PR to handle separately. **★ Lesson: nested parses clobber not just SCOPES but `ORIGINAL_SOURCE` too. The fix itself is correct, but
  it exposes a group of "accidentally PASSing" tests calibrated to the clobber over the years, so proceed incrementally.**
- **2026-06-25 (§D = fixing the `ORIGINAL_SOURCE` clobber + general fix for the exposed phaser block-value bugs)**: resolved the separate bug split off in the previous entry.
  **(1)** `parse_program_partial` (best-effort nested parses for module export scanning / EVAL / pseudo-packages) now save/restores the caller's source state via
  `primary::snapshot_source_state`/`restore_source_state` (`ORIGINAL_SOURCE` + heredoc `LEAKED_REGIONS`) = clobber resolved;
  the using file's `current_line_number` now matches raku. **(2)** General fix for the exposed phaser bugs (**genuine bugs independent of line numbers**): ① a trailing ENTER phaser
  becomes the value of a block/sub/closure/do-block (new OpCodes `PushEnterResult`/`LoadEnterResult` + VM `enter_result_stack` = bridging the ENTER section's value to the end of the
  body. ENTER runs before the value-result baseline is recorded, so placing it directly on the stack is not detected) ② changed the value-determining statement to "the last non-`SetLine` statement" (line-number
  normalization inserts `SetLine` between statements; the trailing `SetLine` of a phaser-only block was a spurious `True`→KEEP misfired, fixed to UNDO). **(3)** Consolidated the 5-fold duplicated BlockScope phaser
  compression (top-level / `Stmt::Block` / do-block / sub body / closure) into the shared `compile_phaser_block_scope(stmts, result_on_stack)` (2 modes: statement=topic /
  do-block=stack). pin `t/enter-phaser-rvalue.t`(18). All of t/ (11502), S04-phasers whitelist (17), the 270 `use Test::Util` whitelist files, and a 1/3 roast
  sample of 428 files — zero regressions. **★ Lesson: the value of a phaser block needs materialization in a separate phase from the ENTER section (bridged to the end of the body via a dedicated stack). do-blocks
  return the value on the stack (`DoBlockExpr` pops), but statement context goes via topic = the same helper branching on mode.**
- **2026-06-25 (§D(b) tree-walk dispatch chain removal = making the unadorned forms of `.starts-with`/`.ends-with` VM-native)**: per-name measurement of method-fallbacks put
  `starts-with`(513) high among catch-all bounces (excluding `name`/`WHAT`/`WHY`/`can`=MOP reflection — not elimination targets, `tap`/`stdout`=concurrency separate axis, the iterator-protocol group=lazy separate axis).
  Cause = `starts-with`/`ends-with` sit on the slow path (`dispatch_prefix_suffix_check`) to support named args (`:i`/`:ignorecase`/`:m`/`:ignoremark`), so **even the general
  `.starts-with($needle)` form without named args bounced to the interpreter**. The `(false,false)` case is a pure prefix/suffix check, so added a
  Str-receiver-gated arm to `native_method_1arg` (methods_narg.rs). The named-arg form is **2 arguments (positional + Pair), so it never reaches the 1-arg native path**
  and naturally falls through to the slow path = byte-identical. A user-defined `.starts-with` is resolved by the VM before native, so no shadowing (the same guarantee as the existing `contains` arm).
  Measured: `$s.starts-with` fallback 2→0. pin `t/starts-ends-with-native.t`(22).
  All of t/ (11572), S32-str/starts-with.t, ends-with.t, 70 string roast files, and a 1/5 roast sample of 257 — zero regressions. **★ Lesson: a pure method placed on the slow path for
  named-arg support can be safely drained by native-gating "only the unadorned 1-arg case" (restricted by receiver type), since named args increase the arity and go down a different path. `substr-eq` (87; complex due to
  Whatever/negative/Failure position resolution) is the next slice candidate.**
- **2026-06-25 (§D(b) tree-walk dispatch chain removal = making `.substr-eq($needle, Int $pos)` VM-native)**: 2nd slice in the same pattern as starts-with. `.substr-eq` sat on the
  slow path (`dispatch_substr_eq`) to support named args (`:i`/`:m`) and Whatever/negative/out-of-range position resolution (Failure generation). **The plain 2-argument form + a non-negative in-bounds Int position + a Str
  receiver** is a pure substring comparison, so added an arm to `native_method_2arg`. Everything else falls through: Whatever/non-Int position (`arg2` not `Int`) → interpreter resolution / negative or out-of-range →
  interpreter Failure / named-arg form (a Pair makes arity 3) → never reaches the 2-arg native path / user-defined `.substr-eq` resolves before native = no shadowing. The 1-argument form (position 0 default) is rare, fallthrough kept.
  pin `t/substr-eq-native.t`(16). All of t/ (11566), S32-str/substr-eq.t, indices.t, index.t — zero regressions. **★ The clean string drains are exhausted**: the simple `comb` form is already native (the 93 in the survey are
  regex/named-arg forms), `trans`(65) is complex (ranges in spec strings `a..z` / list-pair / regex), `Int`/`Num`/`Str.new` are in the completed ctor territory. Remaining categories = the iterator-protocol group (lazy, separate axis) /
  MOP carrier (reflection, not elimination targets) / concurrency (tap/emit, separate axis) — all premised on a different substrate rather than a pure §D(b) drain.**
- **2026-06-25 (§D(b) tree-walk dispatch chain removal = making the `Buf.write-int*`/`.write-uint*`/`.write-num*` family VM-native)**: re-measuring method-fallbacks (aggregated over all 1285 whitelist files)
  revealed the **most frequent clean-drain category** after the string-drain exhaustion = `write-int8/16/32/64/128` (3852 each) + `write-uint*` (2220 each) + `write-num32/64` (492 each) = ~30k fallbacks total.
  The source is **S03-buf/write-int.t / write-num.t** (the `\sigilless` instance form of `existing."$write"($off,$val[,$endian])` and the type-object form of `buf8."$write"(...)` = both **dynamic
  method names** `"$write"`). The existing nativization covered only **static names on mut-bound `$`-vars** (`try_native_buf_mut`, CallMethodMut), so all 3 routes bounced to the interpreter:
  (1) the type-object form (`Value::Package` receiver = returns a fresh buf),
  (2) the `\sigilless`/`BareWord` instance form (via the non-mut dynamic op `exec_call_method_dynamic_op`→`try_native_method`; no writeback target var),
  (3) the `$`-var dynamic-name form (the mut dynamic op `exec_call_method_dynamic_mut_op` = never tries the native fast path and goes straight to `vm_call_method_mut_with_values`). **Fix** = the new pure helper
  `buf_write_int::try_native_buf_write(target, method, args)` handles both the type-object (fresh `make_buf_value`) and instance (in-place commit to the shared cell via `write_back_sharing`→`commit_attrs`
  → observed by all bindings) forms in 1 impl (the byte transform is the existing shared `apply_write_int`/`apply_write_num`). Wired it into **the top of `try_native_method`** (the arity-keyed `native_method_*arg` cannot
  dispatch 3 arguments [offset,value,endian], so a dedicated branch is required) → drains routes (1)(2). Route (3) is drained by calling `try_native_buf_mut` before the generic fork in `exec_call_method_dynamic_mut_op`.
  **Fallthrough kept** = `Blob`/`blob8` (type-object has no such method; instances are immutable and the interpreter raises "Cannot modify immutable Blob"; current behavior byte-identical) / non-Int offsets
  (Whatever position resolution) / negative or out-of-range (`apply_write_int` returns Err→`dies-ok` preserved) / arity≠2,3. **Measured: write-int.t fallback 20240→0, write-num.t 656→0**
  (method-call fallbacks of both fully gone). All of t/ (11615 PASS), all S03-buf green, 108 roast samples zero regressions. pin `t/buf-write-native.t`(27: type-object/scalar/sigilless dynamic name/round-trip/error, all forms). **★ Lesson: dynamic method names
  (`."$name"()`) have 2 dedicated opcodes (`CallMethodDynamic` [BareWord/literal target] / `CallMethodDynamicMut` [`$`/`@`/`%`-var target]), and the latter never tries the native fast path
  = a method already nativized under a static name bounces entirely when called via a dynamic name. Draining a native-method family called via dynamic names requires adding fast-path calls to both dynamic ops. The non-mut
  dynamic form of an instance mutator propagates to `\sigilless`-bound values via an in-place commit to the shared attribute cells (`commit_attrs`) even without a writeback target var (no env insert needed).**
- **2026-06-25 (§D(b) tree-walk dispatch chain removal = making the named-arg/position forms of `Str.contains($needle, $pos?, :i/:m?)` VM-native)**: the first nativization of a
  **named-arg form** of the string-method family that starts-with/substr-eq had conservatively deferred as "named-arg forms stay on the slow path". `S32-str/contains.t` (survey 278) has every test go through
  `invocant.contains(|c)`, **always attaching markings named args (`:i`/`:ignorecase`/`:!i`…)**, so the unadorned 1-arg native form (`native_method_1arg`) is barely reached; the Pair-carrying 2-arg or
  position+Pair 3-arg forms slipped past the arity-keyed dispatch (which stops at `native_method_2arg`, no 3-arg path) and bounced 100% to the interpreter. **Fix** = the new variadic helper
  `native_contains_with_options(target, args)` separates positional/named, folding needle=positional[0], start=positional[1] (Int/Num/Str-parse), and `i`/`ignorecase`/`m`/`ignoremark`
  all into a lowercase comparison (strictly mirroring `Interpreter::dispatch_contains`). Wired immediately before `try_native_method`'s arity dispatch. **Fallthrough kept** = non-Str receivers (**Match invocants** are
  deferred over the separate-axis Match→Str coercion and instance-bypass interaction) / Package needle / BigInt position (overflow→X::OutOfRange goes to the interpreter) / negative or out-of-range positions (X::OutOfRange Failure) /
  unknown named args. Junction needles thread via `contains_value_recursive[_ci]`. **Measured: contains.t fallback 272→140** (remaining 140 = Match invocants, separate axis). All of t/ (11610); 11 contains forms byte-identical
  (including unicode CI and Str positions); roast contains-family samples zero regressions. pin `t/contains-options-native.t`(22). **★ Lesson: starts-with/substr-eq said "named args are deferred", but
  the named-arg form can be the main traffic of real tests (contains.t attaches markings to every form). Wiring a variadic helper (positional/named separation → mirroring the interpreter dispatch) before the arity dispatch
  drains the named-arg forms too. The half with non-Str receivers (Match etc., needing coercion) remains as a separate axis.**
- **2026-06-25 (§D(b) tree-walk dispatch chain removal = making the `:i` named-arg forms of `Str.starts-with`/`.ends-with`/`.substr-eq` VM-native)**: rolling the
  variadic-helper pattern established in the contains slice out to the sibling string methods. `S32-str/{starts-with,ends-with,substr-eq}.t` also attach markings named args to every form (same shape as contains.t), so
  even with the unadorned native forms (starts-with=1arg, substr-eq=2arg) drained, the `:i` forms (arity exceeded by the Pair) bounced 100% to the interpreter. **Fix** = the new helpers `native_prefix_suffix_with_options`
  (starts-with/ends-with) / `native_substr_eq_with_options` (substr-eq) interpret the markings via the shared `split_string_match_args` (positional/named separation; `i`/`ignorecase`=ci, `m`/`ignoremark`=mark; unknown named
  = defer), strictly mirroring `dispatch_prefix_suffix_check`/`dispatch_substr_eq`. Wired immediately before `try_native_method`'s arity dispatch (next to the contains arm). **Fallthrough kept** =
  non-Str receivers (**Match invocants** = separate axis) / Package needle / `:m`/`:ignoremark` (`strip_marks` decomposition is interpreter) / substr-eq's non-Int-non-Str positions (Whatever resolution), negative or out-of-range
  (X::OutOfRange Failure) / unadorned forms (existing 1-/2-arg arms kept). **Measured: starts-with.t 80→42, ends-with.t 80→42, substr-eq.t 86→48** (remaining = Match-invocant `:i` forms, separate axis). All of t/ (11692);
  3 files PASS under the proper harness; 10 named forms byte-identical to raku (including unicode CI and Str positions); whitelist zero regressions. pin `t/starts-ends-substr-eq-named-native.t`(21). **★ Lesson:
  the raku reference implementation throws "Iteration past end of grapheme iterator" on `"abc".ends-with("", :i)` (an empty-needle+ci bug), while mutsu correctly returns True = excluded that form from the pin (mutsu is the correct one).
  ★ The `:m`/`:ignoremark` of string methods is handled separately via `strip_marks` (NFD→combining-mark filter), but these roast tests skip the `:m` forms under a backend≠moar gate, so nativization is unnecessary — defer is sufficient.**
- **2026-06-25 (§D(b) tree-walk dispatch chain removal = making the Match invocants of the 4 string-search methods VM-native)**: the previous 3 slices (contains / starts-with & ends-with / substr-eq) had
  deferred "Match invocants are a separate axis (Match→Str coercion, instance-bypass interaction)", but closer inspection showed **it can be resolved with just a low-risk gate relaxation**: ① `is_native_method("Match", …)` has
  **no entry → false** = Match.contains etc. pass through the instance-bypass block and reach the variadic helpers (immediately before the arity dispatch) ② each helper already obtains the text via `text = target.to_string_value()`
  = for a Match that is the matched substring (identical to `Cool.Str`). ∴ Just relaxing the receiver gate from `Value::Str(_)` only → the new predicate `is_str_or_match_receiver` (Str or `Instance{class_name=="Match"}`) drains
  the Match invocants of all forms of the 4 methods. **A failed match is `Any`/`Nil` (not a Match instance) → misses the gate → the interpreter raises "No such method"**
  (byte-identical). **Measured: contains.t 140→6, starts-with.t 42→8, ends-with.t 42→8, substr-eq.t 48→14** (remaining = bare Match forms [no markings; the 1-/2-arg arms are Str-gated] + the tests' own `.match`/`$*PERL.compiler`).
  All of t/ (11710); 4 files PASS under the proper harness; 9 Match-invocant forms byte-identical to raku (including positions and unicode CI); whitelist zero regressions. pin `t/str-search-match-invocant-native.t`(18). **★ Lesson: even a defer classified
  as a "separate axis" can compress to a one-predicate gate relaxation when you scrutinize the bypass order (if the target class is absent from the `is_native_method` table, it passes through) and the existing coercion (the helper obtains text via `to_string_value`).
  With 4 consecutive string-search slices, the method-fallbacks of `S32-str/{contains,starts-with,ends-with,substr-eq}.t` are nearly exhausted (≤14 each = bare Match forms only).**
- **2026-06-25 (§D state ownership = VM-native dispatch of atomic var/element RMW (`__mutsu_atomic_*`/`__mutsu_cas_*`))**: after the exhaustion of clean method-fallback drains, aggregating
  **function-fallbacks** over the whole whitelist showed
  `__mutsu_atomic_add_var`(640001)/`__mutsu_atomic_post_inc_var`(320038)/`pre_inc`/`post_dec`/`pre_dec`(320001 each)/`__mutsu_cas_var`(282337)/`cas_hash_elem`(30000)/`cas_array_elem`(29420) =
  **~2.5 million fallbacks in total, overwhelmingly dominating all function-fallbacks** (concurrency stress = driven by the `⚛` operators). This is the direct-hit category of §D "state ownership" = atomic ops are RMW against the
  VM-shared `shared_vars` (`RwLock`) store and the per-attribute cell-CAS state (Phase 3). The existing `builtin_atomic_*`/`builtin_cas_*` impls already own and operate on that state, but **the only reach path was
  the generic `call_function` fallback** (`vm_call_function`→`loan_env_for`→ a name match of ~hundreds of arms) (`try_native_function` only calls the pure `native_function`, so `&mut self` atomic ops slipped past). **Fix** = the new
  `try_native_atomic_function(name,args)` (`builtins_atomic.rs`; a 1:1 mirror of the atomic arm group in `builtins.rs`; `cas_*` uses `args.to_vec()`) is called at the top of `try_native_function`
  (before the Instance-arg bail = an Instance value in `cas($x,$old,$obj)` is dispatchable too) when `name.starts_with("__mutsu_atomic_"|"__mutsu_cas_")`.
  `loan_env_for` after the CP-3 collapse is a thin `f(self)` passthrough = env is the same `self.env` on both paths, the builtin is identical = byte-identical.
  **Measured: in atomic-ops.t/atomic.t, the atomic function-fallback markers vanished completely** (cas.t stress = 61 fallbacks out of 95003 opcodes = the remainder is `start`/`await` [concurrency, separate axis] + the user-facing `cas`).
  All of t/ (11725); S17-lowlevel/{cas,cas-loop,cas-loop-int,atomic,atomic-ops}.t all PASS (atomic.t 34/34, atomic-ops.t 28/28 = debug is slow on the `for ^20000` stress but completes); 20 atomic forms byte-identical to raku.
  pin `t/atomic-ops-native-dispatch.t`(20). **★ Lesson: not just method-fallbacks — the function-fallback survey is also a treasure trove for §D. The `__mutsu_*` internal markers (atomic/cas/hyper-prefix), being "not pure (`&mut self`)",
  slipped past `try_native_function`→`native_function`(pure) and all fell into the generic `call_function` fallback. Adding a direct dispatch arm for `&mut self` builtins at the top of `try_native_function` can sweep away the fallbacks of
  huge stress suites (state ownership is already on the VM side = a behavior-invariant path short-circuit).**
- **2026-06-25 (§D state ownership = VM-native dispatch of builtin operator-as-function `infix:<op>(...)`)**: the function-fallback aggregation after atomic showed the `infix:<op>` family = **~5400 fallbacks across 44 operators**
  (`&infix:<+>`, `[+]`/`[*]` reduce, `>>+>>` hyper, and the routine forms that `reduce &infix:<…>` lowers to). Builtin operators are implemented by the native Rust `call_infix_routine` (`&mut self`; dispatches hyper/assignment-metaop/set-op/
  all binary ops), but the only reach path was via `call_function_fallback`'s infix arm (recorded as tree-walk fallback). **Fix** = in both `dispatch_func_call_inner` (the direct Call path) and
  `call_function_compiled_first` (the hyper/reduce/string-regex path), **after trying all user operator resolution (compiled_fns / the non-proto multi fork / `user_function_matches_call` / OTF) and immediately before the terminal fallback**,
  capture `infix:<op>` and call `call_infix_routine(op,args)` directly (U+2212 minus normalized to `-`; arg_sources and `maybe_fetch_rw_proxy` are the same handling as the terminal else = byte-identical). **User overrides strictly respected** =
  user `sub/multi infix:<…>` resolve in the higher branches and never reach the native arm (the pin verifies `infix:<plus>`=103 and a custom `multi infix:<%%%>`). **Measured: direct infix calls 100%→0** (user multis correctly keep the fallback).
  All of t/ (11765); 79 whitelist operator/metaops/junction files zero regressions; 20 infix forms byte-identical to raku. pin `t/infix-function-native-dispatch.t`(22). **★ Widened `call_infix_routine` to `pub(crate)`.
  ★ Lesson: there are 2 fallback-recording lineages — direct Calls at `dispatch_func_call_inner`, hyper/reduce etc. at `call_function_compiled_first` — so the same arm must be placed in both. The key to nativizing user-overridable operators is
  inserting "after all user resolution branches, before the fallback recording" (placing it at the top would shadow user operators). Operator names containing `<`/`>` (`infix:«<»`) hit a separate parser problem = CALL-ME error = excluded from the pin.**
- **2026-06-25 (§D state ownership ③ = VM-native dispatch of the file/FS builtin *function* forms)**: the IO native **method** family was drained (the 2026-06-23 group), but the **function forms** (`slurp($p)`/`open($p,:r)`/`unlink($p)`/
  `spurt`/`close`/`dir`/`copy`/`rename`/`move`/`chmod`/`mkdir`/`rmdir`/`link`/`symlink`) still went via the `call_function` fallback (survey: unlink 1172/open 1017/slurp 569 etc., ~3000 total). These are `&mut self`/`&self` operations against the
  VM-owned `io_handles` store + the filesystem, and the `builtin_*` impls already own the state. **Fix** = the new `try_native_io_function(name,args)` (`builtins_io.rs`; a 1:1 mirror of `call_function`'s IO arms) is called
  **at the same user-override-safe position as infix**: in `dispatch_func_call_inner`, the else-if after all user resolution (after lexical-amp, before the infix arm); in `call_function_compiled_first`, after OTF and before the record.
  **Not placed in `try_native_function`** (in call_function_compiled_first it runs before user resolution [OTF], so it would shadow a non-compiled user `sub slurp` — the difference from atomic, which was safe because `__mutsu_*` cannot be user-defined).
  **Exclusions** = `indir` (executes a callback block), `chdir`/`tmpdir`/`homedir` (process cwd/env side state), `print`/`say`/`note`/`warn`/`sink` (output/blocks). FS functions have no rw params so no arg_sources needed; `maybe_fetch_rw_proxy(true)`
  is the same as the terminal else = byte-identical. **Measured: 0 fallbacks for builtin IO function calls**; user `sub slurp`/`sub unlink` are correctly shadowed by higher resolution (pin-verified). All of t/ (11806); 64 whitelist S32-io/S16-io files zero regressions;
  14 IO forms byte-identical to raku. pin `t/io-function-native-dispatch.t`(14). **★ Lesson: nativization of user-definable builtin names (IO/operators) goes not in `try_native_function` (too early) but at
  "after user resolution, before the fallback record" on both dispatch paths. Only the `__mutsu_*` internal markers (atomic) can sit at the top of `try_native_function` (because they cannot be user-defined). Remaining function-fallbacks = concurrency (await/start —
  start needs `sync_env_from_locals`, separate axis, flaky) / MOP (samewith) / heterogeneous internal markers (feed/assign-lvalue, context-sensitive) = the clean homogeneous drains are exhausted.**
- **2026-06-25 (§D(b) = VM-native dispatch of the pure list/coercion builtin *function* forms (`val`/`list`/`slip`/`hash`))**: a follow-up in the same pattern as the IO function forms. `val` (pure free-fn)/`list`/`slip`/`hash` (`&self`) are
  collection constructors that still went via the `call_function` fallback (survey: slip 3824 [S07-hyperrace stress]/val 1555/list 720/hash 336, ~6400 total). The new `try_native_collection_function(name,args)`
  (`builtins_collection.rs`; a 1:1 mirror of the `call_function` arms) is called at **the same user-override-safe position as IO/infix** (in `dispatch_func_call_inner` after the IO arm, before the infix arm / in `call_function_compiled_first` after the IO arm).
  Pure/`&self` with no rw params = byte-identical. **Measured: 0 fallbacks for builtin val/list/slip/hash calls**; user `sub list` correctly shadowed (pin-verified). All of t/ (11834); 132 whitelist S02/S07-slip/S32-list files zero regressions;
  byte-identical to raku. pin `t/collection-function-native-dispatch.t`(14). **★ Separate axes found via the pin (existing mutsu behavior, unrelated to this change): mutsu's `builtin_list` flattens `list((1,2),3)` but raku gives `((1,2),3)` (non-flattening) /
  a block-scoped `sub list` leaks to the outer scope in mutsu (raku is block-scoped) = both excluded from the pin. ★ The clean function-fallback drains (state-owning: atomic/IO + pure dispatch: infix/collection) are effectively exhausted. Remaining = concurrency
  (await/start/Supply/tap; Promise/scheduler state; flaky) / MOP (samewith; reflection) / user-class construction (new/bless; needs a structural substrate design).**

### Important assessment of the current state (2026-06-08, as of PR-3)
**The cheap sites removable by "just lowering raw dispatch into the unified entry" are exhausted.** The remaining §1/§2 fallbacks
are all premised on structural blockers (② declaration registries / ③ state ownership transfer / first-class containers Phase 2 / lever B) and
will not disappear through individual routing. In particular, the §1 catch-all group (the remaining primary tree-walk) and the native-methods (IO family) are premised on **③**,
the mut-family push/hyper/array-backed on **Phase 2**, and shared push/react on **lever B**.
**The next real progress is the structural refactor of ② or ③, which requires design** (the bidirectional ownership — the VM owning
`interpreter: Interpreter` while the interpreter side also tree-walks over the same state — must be untangled).

## Elimination order (corresponding to PLAN.md ①–⑤)

1. ~~Individual elimination of EASY/MEDIUM §1/§2~~ — **exhausted (consumed in PR-1..3)**. The remainder is premised on the structural blockers below.
2. ② VM ownership of the declaration registries (class/role/enum/subset/sub/token).
3. ③ Transferring ownership of env/type checks/readonly/let to the VM (the biggest mountain. The catch-all group and native-methods disappear here).
4. ④ Final determination of the §C carriers (once ownership moves to the VM, they are mere shared references).
5. ⑤ Deleting `env_dirty`/`saved_env_dirty`/`ensure_locals_synced`/`sync_locals_from_env`.
