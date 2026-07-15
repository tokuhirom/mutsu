# Captured-outer lexical cell sharing — implementation plan (Sub-slice 1b+ / substrate for env_dirty removal)

> **★★ Achieved 2026-06-22: boxing permanently ON = the goal "env↔locals container cell sharing" realized as the default behavior.**
> Flipped `blanket_reconcile_disabled()` / `precise_reconcile_disabled()` to permanent `true` (making cell-boxing the sole
> coherence mechanism). Rationale = the double-OFF survey (roast whitelist 1285, proper harness) plus the full `make test` (`t/` 10135)
> both show 0 failures. Remaining = physical removal of `env_dirty`/`reconcile_locals_from_env_at_site`/`ensure_locals_synced`/`saved_env_dirty`
> (344 uses, already dead code = a behavior-preserving mechanical cleanup). Details = §10.26 / PLAN §1-A.
>
> **Status:** SLICE 1–1.19 DONE (through 2026-06-21, sessions 41–47). §6 first slice (cell-ification of named-sub-captured
> scalars at the decl site) plus §7.1–7.1l (metaop-thunk `Mu` / carrier single-frame / EVAL carrier multi-frame /
> captured-outer container `@`/`%` cell-ification / `X`-cross metaop thunk / nested-method capture / cross-thread shared-var /
> object subscript-assignment invocant slot / substr-rw & subbuf-rw / zip short-circuit / map LAST phaser / undefine() lvalue)
> plus slice 1.17 (proto state-%cache) / 1.18 (caller-frame by-name write) / **1.19 (param default self-scoping = code.t 8)**
> implemented. Every pin **PASSES with blanket reconcile both ON and OFF**. **OFF roast survey (authoritative, §7.2a) = initially 13 → 1 remaining**.
> **destruction.t 3 = resolved by slice 1.20 (#3400)** (retain-on-miss for cross-thread writeback, §7.2c).
> **lazy-lists.t 24-26 = resolved on 2026-06-22** (`.kv`/`.pairs`/`.antipairs` made lazy index-pipes =
> `MapGrepSpec.index_transform`; lazy sources are not eagerly forced). **∴ The deterministic surface of the OFF roast survey
> has been exhausted down to only IO-Socket-Async.t's reactive concurrency flakiness (no deterministic pin possible).**
> Next = the final stage of env_dirty removal (§7.4, hollowing out `blanket_reconcile_if_dirty`).
> Related: [docs/env-locals-coherence.md](env-locals-coherence.md) (§7.3 = the wall for env_dirty removal) /
> [docs/container-identity.md](container-identity.md) (cell infrastructure) / PLAN.md §1-A, §2-C, §2-E.
>
> **★Design decision (session 41) — boxing is mutually exclusive with blanket reconcile, toggle gated**:
> cell boxing and blanket reconcile are **only ever one active at a time** (`box_decl_local_cell` fires only when
> `cell_boxing_active()` = `MUTSU_NO_BLANKET_RECONCILE`). Reason = if you box while reconcile stays ON,
> the cell correctly propagates captured-outer writes that reconcile **drops** in some carriers (`lives-ok {…}` etc.)
> → that propagation **exposes unrelated latent bugs** (e.g. the missing self-ref type check for `&foo = &foo` =
> roast `S06-signature/code.t` test 8 was accidentally PASSing on main due to write-loss). ∴ By default
> (shipping/CI) reconcile carries coherence and stays byte-identical with main; boxing is exercised only under the toggle
> (the pins guarantee the named-sub accumulation surface). When env_dirty is removed, boxing is flipped permanently ON and
> the exposed bugs are fixed one by one. The cell-aware restore for `let`/`temp` (`exec_let_save_op` deref +
> `restore_let_value` write-through) is gated by the same toggle.
>
> **Implementation notes (session 41)**: detection = compile-time. `CompiledCode.named_sub_captures:
> Vec<(free_var_writes, needs_cell_named_sub_free)>` folds in only the **write sets** of direct child named subs
> (`compile_sub_body` pushes after building the cf). `compute_free_vars` records named-sub writes to an own local into
> `needs_cell_named_sub`, and non-own ones into `needs_cell_named_sub_free` (bubbled to ancestors). **Completely separate
> from the closure-driven `needs_cell_locals`** = closures are precisely boxed at their creation op, so reusing that at the
> decl site would over-box unrelated same-named locals (same-named `my` shares the same slot) and break `let`-restore etc.
> (this initially regressed roast `let.t` 4/9/12 = fixed). Boxing happens at the **decl site**, but a captured scalar is
> `needs_env_sync` (non-simple), so box in the **outer wrapper around both the fast and slow inner paths** of
> `exec_set_local_op` (boxing only inside the fast path misses non-simple vars = the initial mistake). Verification toggle =
> `MUTSU_NO_BLANKET_RECONCILE`; the six env_dirty-gated reconciles were consolidated into `blanket_reconcile_if_dirty(code)`
> (so the future env_dirty removal can be done by hollowing out a single method). Lessons: (1) top-level/bare-block
> `my` goes to env (SetGlobal) = cannot be slot-boxed, but captures happen on **block-scoped or sub-body** locals,
> so this is not a problem. (2) Local names are **sigil-less** ("acc" etc.) consistently across `free_var_writes`/`locals`.
> (3) Same-named `my` **shares the same slot** via `alloc_local` = by-name boxing crosses scopes, so refinement via the write set is mandatory.

---

## 0. TL;DR — all the plumbing is in place. The only gap is "detection + boxing"

Removing `env_dirty` requires that env and locals never diverge. The session-40 investigation established that **the
runtime plumbing to achieve this is 100% already present**. The only gap is:

> **Boxing a lexical that is captured-and-mutated by a nested callee (not just a closure but **also** a named sub)
> into the same `ContainerRef` cell in both the owner's local slot and the env entry.**

The `:=` bind already fully implements this "place the same Arc into slot + env + saved frames" pattern (§3), so
the work here is to **apply it to implicit captures without `:=`**.

---

## 1. Why this is the crux of env_dirty removal (the true nature of the §7.3 wall)

The only remaining load-bearing role of env_dirty is supporting "multi-frame captured-outer accumulation":

```raku
my $acc = 0;
sub bump-outer() { $acc = $acc + 10 }   # $acc is a free var; writes env by name via SetGlobal
sub via()        { bump-outer() }
via(); via();                            # raku=20; removing env_dirty gives 10 (accumulation fails)
```

**Mechanism of the wall (confirmed by agent investigation)**: the named sub `bump-outer` is compiled separately with a fresh
compiler (`src/compiler/helpers_sub_body.rs:100`) and accesses `$acc` **as a free var by name in env via `GetGlobal`/`SetGlobal`**.
The top-level escape analysis (`src/opcode.rs:1576-1690` `compute_free_vars`) **only looks at closures (`closure_compiled_codes`)
and not named subs**, so it does not flag `$acc` as `needs_cell` → `$acc` is not cell-ified and depends on env writeback +
(previously reverse pull, now) the env_dirty-gated blanket reconcile.

If `$acc` were a **shared cell**, `bump-outer`'s `SetGlobal("$acc")` would write through the cell (§2-Q1) and the top-level
slot would read the same cell (§2-Q2) → multi-frame accumulation works **without reconcile** → this role of env_dirty disappears.

---

## 2. Make-or-break plumbing (all verified ✓)

| # | Path | Behavior | Location |
|---|------|------|------|
| Q1 | **`SetGlobal`** | When env holds a `ContainerRef`, **writes through the cell** (does not replace the Arc): `arc.lock().clone_from(&val)` | `src/vm.rs:1999-2005` |
| Q2 | **`GetGlobal`** | Reads the inside of the cell via `into_deref()` (does not detach the Arc) | `src/vm.rs:1387` / `value/mod.rs:3070` |
| Q3 | **`$x++`/`$x+=n` (RMW)** | RMW through the cell (atomic-capable, Track C) | `src/vm/vm_var_assign_ops.rs:1889-1902` |
| — | **`SetLocal`** | Writes **through** an existing `ContainerRef` slot: `arc.lock().clone_from(&val)` (when not rebinding) | `src/vm/vm_var_assign_ops.rs:5837-5852` |
| — | **`GetLocal`** | Reads the cell via `into_deref()` | `value/mod.rs` `into_deref` |
| Q4 | **`flush_local_to_env`** | If `locals[idx]` is a `ContainerRef`, propagates the same Arc to env (`set_env_with_main_alias`) | `src/vm/vm_env_helpers.rs:587-603` |

**∴ Both the read and write sides (Local/Global) pass through the cell, and slot→env propagation preserves the Arc. No gaps in the plumbing.**

---

## 3. The precedent to follow — `:=` bind's cell sharing (existing, working)

The `:=` bind **already implements exactly the target "place the same cell into slot+env+saved frames"**. Use it as the template.

### scalar `:=` (`my $a := $b`) — `src/vm/vm_var_assign_ops.rs:6633-6689

```rust
let container = if let Value::ContainerRef(ref arc) = val {
    Value::ContainerRef(arc.clone())          // if already a cell, reuse it (a third alias joins the same cell)
} else {
    val.clone().into_container_ref()           // wrap the value in a new cell
};
self.locals[idx] = container.clone();          // target slot
if let Some(source_idx) = code.locals.iter().rposition(|n| n == &resolved_source) {
    self.locals[source_idx] = container.clone(); // source slot gets the same cell too
    self.flush_local_to_env(code, source_idx);
}
self.env_mut().insert(resolved_source.clone(), container.clone()); // source env
for frame in self.call_frames.iter_mut().rev() {                   // ★propagate to saved frames
    if frame.saved_env.contains_key(&resolved_source) { frame.saved_env.insert(...); }
    for (i, local_name) in code.locals.iter().enumerate() {
        if local_name == &resolved_source && i < frame.saved_locals.len() {
            frame.saved_locals[i] = container.clone();
        }
    }
}
self.set_env_with_main_alias(name, container.clone());  // target env
self.flush_local_to_env(code, idx);
```

### container `:=` (`my @a := @b`) — `src/vm/vm_var_assign_ops.rs:6534-6612` (same shape, for Array/Hash)

**Key lesson (saved-frame propagation)**: it is not enough to place the cell into slot+env — you must **also propagate the
same Arc into the `saved_env`/`saved_locals` of `call_frames`**, or a method return (env restore) rolls the cell back to a
stale value. This is mandatory for new boxing as well.

---

## 4. Current coverage and gaps

### Existing cell-ification (done)
- `:=` bind scalar / array / hash (§3) = env↔locals same cell, **complete**.
- **Scalar locals captured-and-mutated by escaping closures**: `box_captured_lexicals` (`src/vm/vm_register_ops.rs:302-395`)
  cell-ifies the **scalars** in `needs_cell_locals` (escape analysis, `closure_escapes[i]==true`).

### Gaps (uncovered cases needed for env_dirty removal)
Limits of the skip logic in `box_captured_lexicals` (vm_register_ops.rs:334-376) and of the escape analysis:
- **(a) Scalars captured by named subs** (the `$acc` case) = **uncovered**. `box_captured_lexicals` is only called for closures
  (`MakeLambda`/`MakeBlockClosure`), not for named-sub registration (`RegisterSub`).
  The escape analysis also does not include named subs in `closure_compiled_codes`. ← **the multi-frame wall, the first slice**
- **(b) Captured containers `@`/`%`** (both closure / named sub) = **uncovered**. Boxing skips the `@`/`%`/`&` sigils.
- **(c) Scalars with type/where constraints** = **intentionally skipped** (cell write-through would bypass constraint re-checks; kept).
- **(d) Other cases surfaced by removing blanket reconcile** (scoped overlay / carriers etc.).

---

## 5. Verification method — "what breaks when blanket reconcile is removed" is the remaining surface

Temporarily remove the `if env_dirty { reconcile_locals_from_env_at_site }` in `drain_and_reconcile_after_cached_call`
(`src/vm/vm_env_helpers.rs`) (plus the 6 other env_dirty consumer sites) and run `make test` + roast. **Each broken test =
a captured-outer case not yet cell-shared**. Drive these to 0, then remove env_dirty.

> Demonstrated in session 40: removing it breaks 4 subtests of `t/multi-frame-accumulation-coherence.t` (= case (a)).
> Consumer site list: `drain_and_reconcile_after_cached_call` (vm_env_helpers.rs:651), `vm_call_func_ops.rs:605`,
> `vm_call_method_ops.rs:1253`, `vm_call_method_mut_ops.rs:1549`, `vm_closure_dispatch.rs:745`, `vm_helpers.rs:662`.

---

## 6. ★First slice (recommended) — cell-ification of named-sub-captured scalars (the `$acc` case)

The first verifiable step toward env_dirty removal. Attacks the multi-frame wall directly.

### 6.1 Detection — 2 options

**Option A (compile-time, recommended)**: extend `compute_free_vars` (opcode.rs:1576-1690) so that **the
`free_var_syms`/`free_var_writes` of named subs declared in the same scope are folded into the enclosing scope's capture
analysis, just like closures**. This puts `$acc` into `needs_cell_locals`.
- Advantage: rides naturally on the existing `box_captured_lexicals` machinery (a consistent extension of the escape analysis).
- To investigate: whether the named sub's `CompiledCode` (with free_var_writes already computed) is accessible at the point
  the top-level `compute_free_vars` runs. Named subs are compiled when the `sub` declaration statement is compiled
  (`helpers_sub_body.rs`), so it should already exist while the enclosing body is being compiled. Identify the wiring point.

**Option B (runtime, alternative)**: when the `RegisterSub` op executes, scan the registered sub's `free_var_writes` and
cell-ify names matching local slots of the registering frame using the §3 pattern.
- Advantage: the wiring is local.
- Drawback: ordering (a forward-declared/hoisted sub may be registered before `$acc=0`). However the slot already exists as Nil,
  so box a Nil cell → the subsequent `$acc=0` (SetLocal) writes through the cell (verified at vm_var_assign_ops.rs:5837), so it works.

→ **Investigate Option A first; adopt it if the wiring is feasible. Fall back to Option B if difficult.**

### 6.2 Boxing
Cell-ify the detected lexical using the §3 scalar `:=` pattern (same Arc into slot + env + **saved frames**).
Either call `box_captured_lexicals` for named-sub captures too, or box at the decl site (on `$acc`'s SetLocal when `needs_cell`).

### 6.3 Guards (avoiding the perf cliff, following existing rules)
- **Captured AND mutated** only (only free vars included in `free_var_writes` = actually written). Do not box read-only captures.
- **Scalars only** (first slice). Containers `@`/`%` are a separate slice (§7).
- **Skip type/where-constrained** (existing rule, vm_register_ops.rs:349-352).
- Escape-aware. `fib`/`method-call` have no captured-mutated lexicals so they are unaffected (needs timed confirmation, #2746 lesson).

### 6.4 Verification
1. After the first slice, temporarily remove the blanket reconcile in `drain_and_reconcile_after_cached_call` and confirm
   `t/multi-frame-accumulation-coherence.t` **passes** (= evidence that (a) is solved by cell sharing).
2. Restore the blanket and confirm byte-identical behavior (make test 988/9634 maintained).
3. pin = `t/captured-outer-cell-sharing.t` (multi-frame scalar accumulation, nested named subs, recursion,
   ON/OFF reconcile equivalence).

---

## 7. Follow-up slices (up to env_dirty removal)

### 7.0 ★Remaining-surface map (after slice 1, measured 2026-06-21)

Measured after the slice-1 merge with `MUTSU_NO_BLANKET_RECONCILE=1 prove -e target/debug/mutsu t/`:
**23 files still depending on blanket reconcile (= not yet reached by cell sharing)**. All of them **PASS in the default build**
(reconcile carries them). Containers `@`/`%` already work via Arc sharing (no container-only test in the survey). Slice 1's
named-sub scalar accumulation is solved by boxing (the 2 pins PASS under the toggle) so it does not appear on the map.

| Cluster | files | Nature / difficulty of attack |
|---|---|---|
| **Concurrency (~13)** | `supply-{act,close-phaser,elems,multi-whenever-done,quit-handler,sync-infinite-emit,syntax-basic}`, `whenever-{last,quit}-phaser`, `promise-combinator`, `proc-async`(4), `scheduler-cue-times`, `concurrency-basic` | The cross-thread cell-sharing wall. Consistency of the `Arc<Mutex>` cell with `shared_vars`/`clone_for_thread` (§8). **Hardest, defer**. |
| **Carrier (~5)** | `metaop-thunk-captured-outer`(4), `subst-closure-writeback`(3), `eval-carrier-precise`(2), `lazy-reify-captured-outer`(2), `require-expression`(1) | Captured outer writes are carried via a carrier but the cell is not shared multi-frame. |
| **attr/method (~3)** | `attribute-trait-mod`(5, single largest), `methods-instance-regressions`(1), `cross-metaops-regressions`(1) | Captures via method dispatch. |
| **misc** | `gather-lazy`(2), `env-dirty-reconcile-coherence`(1 = the test of reconcile itself) | |

> The map is also output to `tmp/blanket-reconcile-surface.txt` (volatile). Re-measure command is above.

### 7.1 ✅ Slice 1.5 (DONE, session 42) = `metaop-thunk` typed scalars (`Mu` universal relaxation)

`metaop-thunk-captured-outer` (4 fail) is the `my Mu $s; 1 Zand ($s++,)` family = **typed scalars captured by a thunk
(closure)**. Untyped `my $s` PASSES under the toggle (`box_captured_lexicals` boxes it), but the `Mu`-constrained one is not
boxed due to the §6.3/§8 "skip type/where constraints" rule and fails. **`Mu` is the universal type (matches all values), so
write-through bypassing the constraint re-check is harmless** = relax the type-skip in `box_captured_lexicals`
(vm_register_ops.rs:343-353) to only fire when `tc != "Mu"`. Subtests 4/6/8/9 (`Zand`/`Zor`/`Z&&`/`Z||` with `my Mu $s`)
now PASS under the toggle as well. pin=`t/metaop-thunk-captured-outer-coherence.t` (12, PASSES both ON/OFF). make test 9656 /
make roast 1285, no regressions. **Note**: `Any` rejects Junction so it is not universal = the relaxation is `Mu` only. This is
closure-driven, so relaxing only the `box_captured_lexicals` side sufficed; `box_decl_local_cell` (the named-sub path) was not needed here.

### 7.1b ✅ Slice 1.6 (DONE, session 42) = the single-frame part of the carrier cluster (lazy map / gather / subst)

Made the 3 **single-frame** mechanisms of the carrier cluster (where the writing callee is in the same frame as the caller
or is a directly-invoked callee) precise-writeback (pushed to `pending_rw_writeback_sources`; the existing call-site/force-site
drain handles them) = independent of blanket reconcile. **These three were solved by precise-writeback, not cell-ification**
(a lateral rollout of the #3335/#3307 machinery that map/grep already use):

- **lazy map (`@a.map({$c++}).eager`)**: `try_native_array_map`'s `classify_body` over-eagerly treated `$c++`/`$c--`
  (inc/dec of a **plain named** scalar other than the topic) as escaping (`None`) and fell to the slow path, and the slow
  path did not record captured writes. Relaxed to `Expr::Var(_) => Some(false)` (does not mutate the topic, so simple) =
  the native loop handles it and records the free-var write like `$c=$c+1` (vm_native_map.rs:303). Indexed `@a[$i]++`/attr
  `$o.x++` continue to fall back.
- **gather (`gather { ...; $c++; take $_ }`)**: at forcing time `reconcile_caller_after_lazy_force` only reconciled the
  gather body via `blanket_reconcile_if_dirty` (disabled when OFF). Right after the env-merge in both force inners
  (`force_lazy_list_vm_inner` / `_n_inner`), record the gather body's `free_var_writes` via
  `record_eager_block_free_var_writeback` (vm_helpers.rs) = the force-site's existing `apply_pending_rw_writeback` drains precisely.
- **subst (`.subst(/../, { $n++ })`)**: `eval_subst_replacement_cased` propagates captured lexical writes to env but did not
  push to pending, relying on blanket. Push the name to `pending_rw_writeback_sources` at the propagation point
  (methods_string.rs) = the `.subst` call-site drains.

pins (existing) = `t/{lazy-reify-captured-outer,gather-lazy,subst-closure-writeback}.t` (PASS both ON/OFF). make test 9672 /
make roast 1285, no regressions.

### 7.1c ✅ Slice 1.7 (DONE, session 42) = multi-frame cell sharing for the EVAL carrier

The **multi-frame** part of the carrier cluster (`eval-carrier-precise` subtests 3/5, `require-expression` subtest 3) =
**an EVAL carrier writing an ancestor lexical from a descendant frame** (`sub set_x(){ EVAL '$x = 5' }; set_x()`).
Writes inside the EVAL string are **invisible to static `free_var_writes` analysis**, so they cannot be cell-ified at the
owner's decl site. Precise single-frame writeback is also **impossible** = the value is lost on the callee's env restore, and
the drain reads the stale post-restore env (demonstrated). ∴ **Cross-frame cell-ification at EVAL execution time**:
`box_carrier_free_var_writes` (vm_env_helpers.rs) cell-ifies the captured-outer scalars in the EVAL'd code's
`free_var_writes` into the same `ContainerRef` cell across the live `env` + all `call_frames.saved_env`
→ the EVAL's by-name write goes through the cell, and the cell retains 5 even after the owner frame's env restore. **Guards**:
(1) gated by `cell_boxing_active()` (default is a no-op, byte-identical); (2) limited to `__mutsu_in_eval`
(supply/whenever/gather bodies also go via eval_block_value, but Track C concurrency cells are managed separately = don't touch);
(3) sigilless aliases (`__mutsu_sigilless_alias::x`) are skipped (a cell would detach the `\x`→`$a` alias chain);
(4) skip type/where constraints (except `Mu`).
Call site = `eval_block_value` (resolution.rs, after compile, before run). pins (existing) = `t/{eval-carrier-precise-writeback,
require-expression}.t` (PASS both ON/OFF). make test 9679 / make roast 1285, no regressions. OFF survey 20→18.
**Note**: the initial version without `__mutsu_in_eval` (broad) made 14 supply/whenever files OFF-clean but **regressed**
`concurrent-cell-writeback` subtest 4 / `sigilless-params` subtest 3 = a collision with the concurrency cell machinery. Restricting to EVAL resolved it.

### 7.1d ✅ Slice 1.8 (DONE, session 43) = captured-outer container `@`/`%` cell-ification (`attribute-trait-mod` 5 fail)

The true cause of `attribute-trait-mod` (OFF 5 fail, single largest) = **COW detach of a captured-outer container + slot-restore clobber**. A user
`trait_mod:<is>` `push`es `my @noted-names` during class composition, but (1) class registration's `saved_env = self.env.clone()`
raises the array Arc's refcount, and the push's `Arc::make_mut` COW-detaches (env = new Arc B(3), slot = old Arc A(0)). (2) The
subsequent `$obj ~~ Type` smartmatch **restores env from the local slot**, so the stale slot A(0) rolls env back to 0 (with blanket
reconcile ON this never manifests because the slot is pulled from env before the smartmatch). ∴ Make `@noted-names` a **whole-container cell**
so slot==env is preserved: the push goes through the cell, `saved_env.clone()` shares the cell (`Arc<Mutex>`) and does not COW,
and the smartmatch's slot-restore keeps the same cell.

- **Detection (compile-time)**: in-place mutation of a container (`push`/`append`/element-assign) is **not** a `SetGlobal`
  name-write, so it does not appear in `free_var_writes`. Accounted separately in a new field `CompiledCode.free_var_container_writes`
  (`op_container_mutate_const_idx` extracts `@`/`%` non-own names from `CallMethodMut` + mutating method names /
  `IndexAssign*Named` / `ArrayPush`), and `compile_sub_body` pushes the named sub's `free_var_writes ∪ free_var_container_writes`
  into `named_sub_captures` → the existing fold puts it into `needs_cell_named_sub`. **`free_var_writes` itself is unchanged =
  no impact on the default build's precise-writeback drain.**
- **Boxing**: lift the `@`/`%` skip in `box_decl_local_cell` and dispatch to `box_decl_local_container_cell` (wraps Array/Hash
  in a `ContainerRef` cell and places it in slot+env; typed containers are skipped). Write-back (`try_native_array_mut`/`try_native_hash_mut_bound`/
  `env_root_descended_mut`) and read (`into_deref` in `GetArrayVar`/`GetHashVar`) are already cell-aware for `:=`-bound containers.
- **★Broad boxing is not viable (decont leaks)**: a spike that boxed "all `@`/`%` decls" regressed ~12 files OFF
  (aliased-container-mutation / constant-hash-element-ro / hash-push-seq-of-pairs / quanthash-hyper-funcop etc.; the cell leaks
  through slice/`.kv`/raw-items reads). ∴ **Precise detection (only named-sub captured-and-mutated containers) is mandatory**. The precise version has 0 decont leaks.
- pin=`t/captured-outer-container-cell-sharing.t` (9; trait_mod push/hash-elem, named-sub accumulation, PASSES both ON/OFF).
  OFF survey 17 files (the 5 attribute-trait-mod fails resolved).

### 7.1e ✅ Slice 1.9 (DONE, session 43) = captured-outer scalar write in `X`-cross metaop thunks (`cross-metaops` subtest 2)

`Nil Xorelse ($t = $_,)` (`X` cross meta over a short-circuit op) compiles the RHS into an **immediately-invoked thunk**
`__mutsu_cross_shortcircuit("orelse", Nil, AnonSub{$t=$_})`. A closure passed as a call arg is treated as non-escaping by
the escape analysis → `box_captured_lexicals` does not box it → the thunk's captured-outer write (`$t`) does not reach the
caller slot when OFF. **Solved by precise-writeback, not cell-ification** (same shape as slice 1.6's lazy-map/gather/subst):
`builtin_cross_shortcircuit` pushes the thunk's `compiled_code.free_var_writes` onto `pending_rw_writeback_sources` via
`record_eager_block_free_var_writeback` after thunk execution (only when `thunk_ran`) → the call-site of
`__mutsu_cross_shortcircuit` drains with the existing `apply_pending_rw_writeback`.
**Ungated (works both ON/OFF, idempotent with reconcile)**. pin=`t/cross-metaop-thunk-captured-writeback-coherence.t`
(6; Xorelse/Xandthen/Xor; PASSES both ON/OFF). make test, no regressions. OFF survey 17→16.
**Note (separate out-of-scope bugs)**: (1) the list-iteration count of `(1,2,3) Xandthen (...)` (raku=3, mutsu=2) is a separate
bug in the cross_shortcircuit loop (fails even ON, unrelated to captured writes). (2) The parser quirk where `$x + 1` (after
`$x=11`) returns 2 is also a separate issue. Both are out of scope for this slice.

### 7.1f ✅ Slice 1.10 (DONE, session 44) = nested-method capture (`methods-instance` subtest 3)

The case where the **method declared inside a method body** — `method foo { my $a = 42; method bar { $tracker = $a } }` —
is called as `.bar` after `.foo`. `bar`'s captured-outer write (`$tracker`) does not reach the caller slot when OFF and goes stale.
**True cause = the dispatch path**: the nested-declared method is registered when foo runs, via RegisterSub, as a
**`&bar` sub with a captured env**, and `$d.bar` goes through neither the compiled method merge path
(`call_compiled_method` / `merge_method_env`) nor closure dispatch (`call_compiled_closure_with_topic`), but the slow path
`call_method_with_values` → **`call_sub_value(merge_all=true)`** (tree-walk). `call_sub_value` correctly merges captured-outer
scalar writes into `merged` (the caller env), but **does not refresh the caller's local slot** = the slot is pulled from env
only when blanket reconcile is ON.
(For class-level methods, `$Foo++` uses the compiled path = already OFF-viable via #3322's `merge_method_env`
`changed_caller_locals` recording. The gap is only the nested-declared method's interpreter dispatch path.)

- **Fix (precise-writeback, ungated)**: (1) in the merge loop of `call_sub_value` (resolution.rs), collect the
  **captured-outer scalars that changed** relative to the body-entry snapshot (`body_entry_env`) (Bool/Int/Num/Str/Rat,
  present in the caller env) and push to `pending_rw_writeback_sources` (both merge_all / non-merge_all branches; same shape
  as the existing precise scalar-changed condition).
  (2) Change the call-site tail of the CallMethod op (vm_call_method_ops.rs) from `blanket_reconcile_if_dirty(code)` →
  `drain_and_reconcile_after_cached_call(code)` (= `apply_pending_rw_writeback` + blanket). **This op previously did not
  drain pending** (the `merge_method_env` comment "the CallMethod op writes back to the slot" expressed an intent the
  implementation had not caught up with = a latent gap fixed at the same time). ON: blanket is a superset so byte-identical;
  OFF: only the precise drain takes effect.
- pin=`t/nested-method-captured-writeback-coherence.t` (9; given/topic, explicit invocant, RMW accumulation, +=, string,
  enclosing lexical read + outer write, multiple outers, subsequent-expression coherence, intervening call; PASSES both ON/OFF).
  make test 9727 / make roast, no regressions. OFF survey 16→15 (~13 concurrency remaining).

### 7.1g ✅ Slice 1.11 (DONE, session 44) = cross-thread shared-var writeback (the deterministic part of the concurrency cluster)

Of the ~13 concurrency cluster, `promise-combinator` and `scheduler-cue-times` are **deterministic**, not flaky, OFF
dependencies (OFF=1 reproduces 4/4, ON=0). `my $seen = []; Promise.allof(start { cas $seen, -> @c { flat @c, 1 } }).result;
is ~$seen, '1'` = the worker `start` block's `cas` (atomic array CAS; writes to the `__mutsu_atomic_arr::$seen` shared store)
does not reach the parent's `$seen` **local slot** after `.result` (await). `sync_shared_vars_to_env` (runtime/mod.rs)
writes cross-thread dirty keys back to env (resolving `__mutsu_atomic_arr::`/`__mutsu_atomic_hash::`/`__mutsu_atomic_name::`),
but **does not refresh the caller slot** = pulled only with blanket reconcile ON.

- **Fix (precise-writeback, ungated)**: in the `updates` application loop of `sync_shared_vars_to_env`, push each synced key
  to `pending_rw_writeback_sources` → the **CallMethod call-site of `.result`/await drains** (already converted to
  `drain_and_reconcile_after_cached_call` in slice 1.10, so no extra wiring needed). ON: env_dirty→blanket superset =
  byte-identical; OFF: only the precise drain. Env is the source of truth after a cross-thread sync, so slot=env is always
  the coherence direction = safe.
- **★A full implementation of cross-thread cells (doc §8) was not needed**: basic cross-thread captured writes
  (`start { $x = v }; await`) already work both ON/OFF via shared_vars (confirmed with a deterministic probe). The
  deterministic OFF dependency was **only the atomic CAS sync→slot writeback**, solved by simply putting the sync names onto
  slice 1.10's drain substrate. The remaining OFF dependencies in the concurrency cluster are the **flaky reactive family**
  (supply/whenever timing dependencies; do not show up in `^not ok`), not a deterministic substrate.
- pin=`t/cross-thread-shared-var-writeback-coherence.t` (6; cas array/scalar/hash, accumulation, subsequent expression,
  intervening; PASSES both ON/OFF). make test 9739 / make roast (to confirm). **Deterministic OFF-only fails (`^not ok`) = reached 0**.

### 7.1h ✅ Slice 1.12 (DONE, session 45) = invocant slot writeback for object subscript assignment (`parametric-role-of-type`)
Resolved `parametric-role-of-type` (deterministic abort when OFF; ran 11/14; test 12). The true cause was not the initial
"COW detach of the typed array attribute" diagnosis but **the invocant slot not being refreshed on object subscript assignment**.
Minimal reproduction =
`role ER[::T] does Positional { has ER[T] @!c handles <AT-POS ASSIGN-POS BIND-POS> }; my $e = ER[Int].new; $e[0] = ER[Int].new; say $e[0].WHAT`
gives ON=`(ER)` / OFF=`(Any)`. `$e[0] = v` goes `exec_index_assign_expr_named_op` → ASSIGN-POS dispatch →
`assign_method_lvalue_with_values` (the handles delegation path at methods_mut.rs:1813) mutates the delegate container and
writes the updated instance to `env[$e]` via `write_back_sharing`, but **does not refresh the caller's local slot**. The default
build never manifests this because blanket reconcile pulls `$e` from env; under OFF (cell boxing) the slot remains the stale
instance → AT-POS reads an empty `@!c`.
- **Fix (precise, ungated)**: at the tail of `exec_index_assign_expr_named_op`, if `env[var]` after the subscript assignment
  is `Instance`/`Mixin` (= the case where the object subscript assignment dispatched ASSIGN-POS/ASSIGN-KEY), write through to
  the local slot via `locals_set_by_name`. Plain Array/Hash element assignment already updates the slot on the fast path and
  never arrives here as Instance/Mixin, so it does not fire. ON is an idempotent superset of blanket reconcile = byte-identical.
  **Side effect**: also fixes the pre-existing bug where `$p[0]=v` on a plain `role P does Positional { has @!c handles
  <AT-POS ASSIGN-POS> }` returned `Nil` (both ON/OFF).
- pin=`t/positional-role-attr-writeback-coherence.t` (7; Positional/parametric/Associative subscript assignment, BIND-POS; PASSES both ON/OFF).
  make test 9755. **Note**: a delegated mutating *method call* (`$q.push(5)` delegating to `@!c`) failing both ON/OFF is a
  separate pre-existing bug (invocant slot not refreshed after a method call; toggle-independent) = out of scope for this
  slice and excluded from the pin.

### 7.1i ✅ Slice 1.13 (DONE, session 45) = substr-rw / subbuf-rw lvalue slot writeback (largest OFF chunk, 12 fails)
`substr-rw($s, ...) = v` / `subbuf-rw($buf, ...) = v` do not modify `$s`/`$buf` when OFF (stays `gorch ding`; raku=
`gloop ding`). Path = the lvalue assign is a `__mutsu_assign_named_sub_lvalue` Call → `assign_named_sub_lvalue_with_values`
(builtins_lvalue.rs:258/281) locates the target var by env scan and delegates to `assign_method_lvalue_with_values` →
mutates the string/buf and writes to `env[$s]` but **the caller local slot is not refreshed**. The `lvalue_writeback_target`
mechanism of `exec_call_func_op` is exclusive to `__mutsu_assign_method_lvalue`/`__mutsu_index_assign_method_lvalue`
(args[4]=target) and does not cover named-sub lvalues.
- **Fix (precise, ungated)**: in the substr-rw/subbuf-rw branch, push the resolved target var name to
  `pending_rw_writeback_sources`. `exec_call_func_op` **always** calls `apply_pending_rw_writeback(code)`
  (vm_call_func_ops.rs:597), so the updated env value is drained precisely to the slot (no blanket pull needed). ON is an
  idempotent superset = byte-identical.
- pin=`t/substr-rw-lvalue-writeback-coherence.t` (6; substr-rw/from-only/accumulation/bound proxy/subbuf-rw; PASSES both ON/OFF).
  make test 9754. `S32-str/substr-rw.t` 46/46 and `S03-operators/buf.t` also PASS ON/OFF (**2 files** cleared from the OFF survey).

### 7.1j ✅ Slice 1.14 (DONE, session 45) = zip short-circuit topicalizing thunk writeback (`zip.t` 68/71)
The topicalize thunk write of `23 Zandthen ($side-effect = $_,)` is lost when OFF (OFF=0; raku/ON=23). `Zandthen`/`Zorelse`
evaluate the thunk via `builtin_zip_shortcircuit_topic` (builtins_feed.rs:299) / `builtin_zip_shortcircuit`, but unlike
`builtin_cross_shortcircuit` (slice 1.9, X-cross) **they did not record captured-outer writes** = an immediately-invoked
closure the escape analysis does not box → only blanket reconcile was carrying it. Fix = both functions call
`record_eager_block_free_var_writeback(&code, &params)` after thunk execution, pushing to pending → the
`apply_pending_rw_writeback` at the `__mutsu_zip_shortcircuit*` call site drains (same shape as X-cross). ON is an idempotent
superset = byte-identical.
pin=`t/zip-shortcircuit-topic-writeback-coherence.t` (5; Zandthen/Zorelse topicalize + increment; PASSES both ON/OFF).
make test 9769. `S03-metaops/zip.t` PASSES ON/OFF (**1 file** cleared from the OFF survey).

### 7.1k ✅ Slice 1.15 (DONE, session 45) = `.map` block LAST phaser writeback (`map.t` 62)
The LAST phaser write of `(^20).map({ LAST $ranLAST=True; last if $_==10; $_ }).iterator` is lost when OFF (OFF=Nil; ON=True).
True cause = the eager map loop (around `resolution.rs:2123`) compiles/runs the LAST phaser body **separately** from the map
body (:2253-2258), and the immediately following `record_eager_block_free_var_writeback(&code, ...)` (:2276) records **only
the map body's `code`** = it does not pick up the phaser's `$ranLAST=True`. Fix = add
`vm.record_eager_block_free_var_writeback(&phaser_code, &[])` right after phaser execution → pending push → the call site's
`apply_pending_rw_writeback` drains. ON is an idempotent superset = byte-identical.
pin=`t/map-last-phaser-writeback-coherence.t` (4; last/natural-end/eager; PASSES both ON/OFF). `S32-list/map.t` PASSES ON/OFF.

### 7.1l ✅ Slice 1.16 (DONE, session 45) = `undefine()` lvalue slot writeback (`undef.t` 85)
`undefine($x) = v` (rw) does not modify `$x` when OFF (OFF=foo; ON/raku=bar). **Same path** as slice 1.13's substr-rw =
the `undefine` branch of `assign_named_sub_lvalue_with_values` (builtins_lvalue.rs:250) writes only `env[$x]` without
refreshing the slot. Fix = push the target var name to `pending_rw_writeback_sources` before the env write → the call site's
`apply_pending_rw_writeback` drains.
pin=`t/undefine-lvalue-writeback-coherence.t` (3). `S32-scalar/undef.t` PASSES ON/OFF.

### 7.2 Follow-up slices (beyond that)
- **Invocant writeback for delegated mutating method calls** (pre-existing, toggle-independent): when `$q.push(5)` is
  delegated to `@!c` via `handles`, the attr mutates but the caller's `$q` slot/instance is not refreshed after the method
  call and accumulation breaks (fails both ON/OFF). Not needed for env_dirty removal (broken even ON = not a deterministic
  dependency of the OFF survey), but fixable separately as a general bug.
- **Cell-ification of containers `@`/`%` beyond named subs** (if needed): closure-captured containers currently work via Arc
  sharing. Relaxing the `@`/`%` skip in `box_captured_lexicals` requires the §8 decont audit as a prerequisite (broad boxing
  demonstrably regressed ~12 files = precise-only is mandatory).
- **Remainder of the concurrency cluster** (supply/whenever/react ~11): §7.1g resolved the deterministic part
  (the atomic-CAS sync of promise-combinator/scheduler-cue-times). The remainder is the **flaky reactive family**
  (supply/whenever timing dependencies; does not show in `^not ok`, manifests as abort/timeout) = no deterministic OFF pin can
  be written, so the toggle survey cannot chase it. Realistically, verify actual behavior at the final stage of env_dirty
  removal (hollowing out `blanket_reconcile_if_dirty`). Basic cross-thread captured writes already work both ON/OFF via
  shared_vars (a full cross-thread cell implementation turned out to be unnecessary).

### 7.2a ★OFF roast survey (session 45, 2026-06-21, authoritative) = the true remaining surface for env_dirty removal
The earlier "deterministic OFF dependencies = reached 0" was an underestimate based on a **`^not ok` survey of t/ only**.
Actually running `MUTSU_NO_BLANKET_RECONCILE=1 make roast` (release, full whitelist 1285) showed **13 files failing
deterministically when OFF** (all pre-existing = the same subtests fail OFF on the main baseline too; unrelated to this
slice's changes = confirmed by debug comparison). This is the remaining surface that must be cleared to unlock the physical
removal of env_dirty (§7.4) (initially 13 → slice 1.13 cleared substr-rw.t/buf.t, slice 1.14 zip.t, slice 1.15 map.t,
slice 1.16 undef.t, slice 1.17 proto.t/subsignature.t, slice 1.18 caller.t/callframe.t → **4 remaining**):

| file | failed subtests | Estimated category | Status |
|------|-----------------|--------------|------|
| ~~S32-str/substr-rw.t~~ | 1, 8-9, 16, 24-25, 33-38 | substr-rw lvalue slot writeback | ✅ slice 1.13 |
| ~~S03-operators/buf.t~~ | 38 | subbuf-rw lvalue slot writeback | ✅ slice 1.13 |
| S02-types/lazy-lists.t | 24-26 | lazy list captured-outer | |
| ~~S03-metaops/zip.t~~ | 68, 71 | Z-cross topicalizing thunk writeback | ✅ slice 1.14 |
| ~~S02-names/caller.t~~ | 9 | caller-frame by-name write slot writeback | ✅ slice 1.18 |
| ~~S06-advanced/callframe.t~~ | 12 | caller-frame by-name write slot writeback | ✅ slice 1.18 |
| ~~S06-multi/proto.t~~ | 21 | caching proto `state %` writeback | ✅ slice 1.17 |
| ~~S06-multi/subsignature.t~~ | 54 | caching proto `state %` writeback | ✅ slice 1.17 |
| ~~S06-signature/code.t~~ | 8 | `&`-param default self-scoping | ✅ slice 1.19 |
| ~~S12-construction/destruction.t~~ | 3 | cross-thread worker-DESTROY captured-write (retain-on-miss sync) | ✅ slice 1.20 |
| ~~S32-list/map.t~~ | 62 | `.map` block LAST phaser writeback | ✅ slice 1.15 |
| ~~S32-scalar/undef.t~~ | 85 | undefine() lvalue slot writeback | ✅ slice 1.16 |
| S32-io/IO-Socket-Async.t | 5, 7 | reactive concurrency (suspected flaky) | |

**Triage (sessions 45–46, measured)**:
- ~~**map.t 62**~~ ✅ **cleared by slice 1.15** (§7.1k). The LAST phaser body of `.map({ LAST $x=True })` is compiled and
  executed **separately** from the map body (`resolution.rs:2253`), so the map body's `record_eager_block_free_var_writeback`
  (:2276) did not pick up the phaser's write. Solved by adding `record_eager_block_free_var_writeback(&phaser_code, &[])`
  right after phaser execution.
- **lazy-lists.t 24-26 (`.kv`/`.pairs`/`.antipairs` is lazy) = an exposed laziness bug, not a writeback issue** (measured
  ON=1/OFF=0/raku=1). `.kv` **eagerly forces** `make-lazy-list = gather { take ...; $was-lazy = 0 }.lazy`, running
  `$was-lazy=0` (mutsu's `.kv`/`.pairs`/`.antipairs` are non-lazy). ON matched raku by accident via write-loss; OFF
  propagates correctly via slice 1.6's gather writeback and **exposes the latent laziness bug** (a classic instance of doc
  §7.3 lesson #3). ∴ The fix is **true lazification** of `.kv`/`.pairs`/`.antipairs` (L family, separate axis) =
  precise-writeback cannot solve it. The laziness fix is a prerequisite for env_dirty removal.
- ~~**undef.t 85**~~ ✅ **cleared by slice 1.16** (§7.1l, the `undefine()` lvalue on the same path as substr-rw).
- ~~**proto.t 21 + subsignature.t 54**~~ ✅ **cleared by slice 1.17 (1 fix, 2 files, PR #3389)**. The caching proto's
  (`proto cached($a){ state %cache; %cache{$a} //= {*} }`) cache did not accumulate across calls, and the multi was
  re-dispatched every time (OFF=`aba` / raku=ON=`ab`). True cause = **dual-store divergence of a hash element-assign whose
  RHS evaluates a `{*}` redispatch**: `{*}`'s (`call_proto_dispatch`) `restore_env_preserving_existing` (dispatch.rs:2074)
  swaps `self.env` wholesale, **detaching** env's `%cache` Arc from the proto body's local-slot Arc (strong_count→1).
  `try_fast_hash_element_assign` (vm_var_assign_ops.rs:2606) does not update the local slot when strong_count==1 (it only
  mutates env in place), so the slot remains a stale empty hash, and the subsequent `sync_env_from_locals` (run_inner)
  writes the stale slot back to env → the `state` persistence saves an empty hash → cache lost. Fix (ungated, precise) =
  at the tail of the fast hash assign, even when strong_count==1, if the local slot **exists** (= by definition necessarily
  diverged) mirror the post-assignment env value to the slot (env-only vars like `%*ENV` have no slot = no-op; default is
  redundant with blanket reconcile = byte-identical). `state @`-array element-assign (`@cache[$i] //= {*}`) + in-place push
  were already coherent and not affected. Lesson: run_inner's state persist runs in the order `sync_state_locals` (env
  priority; 476) → `sync_env_from_locals` (480), so reordering (env flush first) backfires (the stale local clobbers the
  fresh env) = fixing the divergence at the source is the right answer. pin=`t/proto-state-cache-writeback-coherence.t`
  (6; PASSES both ON/OFF). make test 9788.
- ~~**caller.t 9 + callframe.t 12**~~ ✅ **cleared by slice 1.18 (1 fix, 2 files, session 46)**. Writing a caller-frame
  lexical by name — `$CALLER::x = v` (rw dynamic-var) / `callframe(d).my.<$x> = v` — is stale when OFF (write succeeds ON).
  True cause = `set_caller_var` (mod.rs:5327) writes to `caller_env_stack[idx]` and the current env, and
  `pop_caller_env_with_writeback` (mod.rs:5277) propagates dynamic vars to the restored caller env at return, but
  **the caller's local slot is not updated** = OFF reads the stale slot. Fix = push the bare names written by
  `set_caller_var` onto a **new list `pending_caller_var_writeback`**, drained by the call-site's
  `apply_pending_rw_writeback` from `env[name]` → slot. ★Reason for separating it from `pending_rw_writeback_sources`
  (drop-on-miss) = the caller-frame's slot is several frames up, and if the writer makes a **deeper** call before returning
  (`f(){ callframe(1).my.<$x>=v; g() }`), the pending would be consumed one frame too early at g()'s call site →
  **retain-on-miss** (if the slot is not in the current code, keep instead of discarding, carrying it up to the owning
  frame). Caller lexicals that are not `is dynamic` still die as before.
  pin=`t/caller-frame-write-slot-coherence.t` (5; PASSES both ON/OFF). make test 9799.
- ~~**code.t 8 (`&`-param)**~~ ✅ **cleared by slice 1.19 (session 47, separate PR)**. The default-scoping bug of
  `sub foo(&foo = &foo){...}` = **an exposed bug, not a writeback issue** (ON accidentally PASSes because the captured write
  is dropped by reconcile in the `lives-ok { foo }` carrier; OFF propagates correctly and exposes it). True cause = when the
  parameter's default expression is evaluated, **the parameter itself is not yet in scope**, so `&foo` (the default RHS)
  resolved to the outer registered sub `foo` (raku resolves it to the param `&foo` itself = undefined).
  Fix = consolidate both default-eval sites of `bind_function_args_values` (binding.rs ~770 named / ~1500 positional) into a
  new helper `eval_param_default` that **pre-binds the parameter's own name in env to an undefined type object (or Nil)
  immediately before** evaluating the default expression (self-reference resolves to the undefined parameter rather than the
  outer = Raku's "a param is in scope inside its own default" rule). Earlier params (`$b = $a`) are already bound in a prior
  iteration so unaffected; differently-named outers (`$z = $y`) are also unaffected. pin=
  `t/param-default-self-scoping.t` (7; PASSES both ON/OFF). make test 9819.
- ~~**destruction.t 3**~~ ✅ **cleared by slice 1.20 (session 48, retain-on-miss cross-thread sync)**. The worker-thread
  captured write of `submethod DESTROY { $a++ }` does not reach the top-level slot. True cause = §7.2c's "cell-detachment"
  was a **misdiagnosis** (corrected in #3398; the real cause is misuse of the **drop-on-miss** list for cross-thread
  writeback). Confirmed by measured trace: the worker writes `$a` to 1 and marks `shared_vars_dirty` → `await`'s
  `sync_shared_vars_to_env` (mod.rs:6420) pushes `["a","@order"]` to pending and sets `env["a"]=1` → but then
  `run_pending_instance_destroys()` inside `await` (builtins_system.rs:1796) **dispatches DESTROY with `locals=[]`**, and its
  tail `apply_pending_rw_writeback` (drop-on-miss) **consumes and discards** the pending → by the time the top-level frame
  (`locals=["a","@order","b0"]`, owning the "a" slot) drains, `sources=[]`. **Fix** = change the push target of
  `sync_shared_vars_to_env` from the **drop-on-miss `pending_rw_writeback_sources` → the retain-on-miss
  `pending_caller_var_writeback`** (the owning slot of a cross-thread synced var is several frames up = the same shape as
  caller-var, so retain-on-miss is correct). Reuses the same list as slice 1.18's caller-frame writeback = intermediate
  DESTROY frames retain (no slot), and the top-level drains and refreshes the slot.
  pin=`t/destroy-cross-thread-writeback-coherence.t` (5; PASSES both ON/OFF).
  - Plus separate axes: lazy-lists.t 24-26 (laziness bug, L family) / IO-Socket-Async.t 5,7 (flaky).

**Remaining = only the 2 separate-axis items (lazy-lists laziness, IO-Socket-Async flaky). The pure writeback-coherence
surface is exhausted (slice 1.18 was pure writeback, 1.19 an exposed bug, 1.20 cross-thread DESTROY writeback).
§7.4 (env_dirty removal) comes into range after true lazification of lazy-lists (L family, separate axis).**

### 7.2c ✅ destruction.t 3 — cross-thread (worker DESTROY) captured-write (investigated session 47, resolved session 48 = slice 1.20)

**★Resolved (slice 1.20, session 48).** The true cause was **misuse of the drop-on-miss list** for cross-thread writeback
(see the slice 1.20 row in §7.2a). Fix = change `sync_shared_vars_to_env`'s push target from `pending_rw_writeback_sources`
(drop-on-miss) → `pending_caller_var_writeback` (retain-on-miss). Note that the "cell-detachment" diagnosis from the first
half of session 47 was corrected as wrong in #3398 (measurement showed `$a` was never cell-boxed; the
`box_decl_local_cell`/`box_captured_lexicals` probes never fired) = the true cause is cross-thread. What follows is the
investigation record from that time (historical reference).

**Minimal reproduction (deterministic; reliably fails with `MUTSU_NO_BLANKET_RECONCILE=1`)**:
```raku
my $a = 0;
my @order;
class Foo { submethod DESTROY { $a++ } }
class Bar { submethod DESTROY { push @order, "x" } }
my $b0 = Bar.new; $b0 = Nil;
await start {
    loop {
        $*VM.request-garbage-collection;
        my $foo = Foo.new;
        my $bar = Bar.new unless +@order;   # conditional creation
        last if $a && @order;
    }
};
say "a=$a";     # OFF: a=0 (ON/raku: a=1)
```

**Most important fact: `start` is a separate thread**. `builtin_start`→`spawn_callable_promise` (builtins_system.rs:106)
launches the worker with **`clone_for_thread()` + `spawn_user_thread`** = **DESTROY fires on the worker thread (a cloned
interpreter)**. Env propagates worker→parent (slice 1.11's shared_vars machinery), but the parent's **local slot** needs a
separate refresh.

**Isolation (OFF)**:
- Simple cross-thread write `await start { $x = 5 }` → ✅ `x=5` (already works since slice 1.11).
- Single DESTROY-in-thread `await start { my $f=Foo.new; $f=Nil; $*VM.request-garbage-collection }` → ✅ `x=5`.
- **What fails is loop + multiple captured vars (`$a` scalar + `@order` array) + conditional creation** (d10/min4/min10).
  No loop / single var is OK.

**True cause (confirmed by measurement; probed at the parent's await call-site)**: at the point of
`exec_call_func_op`→`dispatch_func_call_inner`→**L597 `apply_pending_rw_writeback(code)`**:
- `code.locals = ["a","@order","b0"]` (top-level frame) / `find_local_slot("a") = Some(0)` (**the slot exists**) /
  `env.get("a") = Int(1)` (**env is correctly propagated**).
- But `pending_rw_writeback_sources` / `pending_caller_var_writeback` are **empty** = the drain never runs and slot[0] stays 0.
- ∴ **The only missing piece is "recording the captured scalar `a` written by the worker into the parent's pending
  writeback"**. The single-write version works because slice 1.11's `sync_shared_vars_to_env` (mod.rs:6412; pushes dirty keys
  to `pending_rw_writeback_sources`) carries "a". In the loop+multiple-var version, **"a" is not carried as dirty** (either
  the `last if $a` read inside the loop consumes the dirty flag, or the multi-var dirty tracking only carries a subset — to
  be determined).

**Attempted fixes that did not work (session 47; all reverted)**: (1) recording changed captured scalars at the DESTROY merge
(class.rs:1421) into `pending_rw_writeback_sources`/`pending_caller_var_writeback` + a loop-boundary drain → ❌ the recording
goes into the **worker interpreter's pending** and vanishes at join (the drain only happens on the worker frames with
`locals=[]`/`["foo","bar"]`, "a" slot absent so retain→lost). (2) cell write-through at the DESTROY merge → ❌ env is not a
cell in the first place, just a plain Int (`saved=Some(Int(0))`) = no cell, so it fails.

**∴ The core of the next session = extending cross-thread writeback recording (slice 1.11 family; expected to be solvable
locally)**: if `sync_shared_vars_to_env` (called by await at builtins_system.rs:1791) pushes the captured-outer scalars
mutated in the worker to `pending_rw_writeback_sources` **without omission**, the parent's existing await call-site drain
(L597, which can reach the top-level slot) refreshes the slot. **First identify: the path through which "a" falls out of
shared_vars_dirty in the loop+multiple-var case** (dirty cleared by the `$a` read inside the loop? only a subset of vars in
the worker's final sync?). This requires **no cell substrate** = it is a fix for dropped dirty/pending recording of
cross-thread shared vars. Both env and slot are measured to be reachable at the await call-site, so supplying the pending
recording alone makes it pass. destruction.t 3 is the pin. (Separate axes: lazy-lists.t laziness / IO-Socket-Async flaky are
unrelated.) (Auxiliary option B = an await/loop-boundary top-level drain becomes unnecessary once cell propagation lands.)

### 7.2b ✅ proto `{*}` redispatch + `state` var coherence (proto.t 21 / subsignature.t 54) = resolved by slice 1.17
**The actual root cause differed from the initial guess (`restore_env_preserving_existing` rolling back state).** A normal
sub with a plain `state %h` persists correctly even OFF (`counter(){state %seen; %seen<x>++}` → 1 2 3) = the state machinery
itself is OK. A `state $n` scalar also persists correctly in a proto body. **The problem is specific to the
`%cache{$a} //= {*}` hash element-assign**: `{*}`'s (`call_proto_dispatch`) `restore_env_preserving_existing`
(dispatch.rs:2074) swaps `self.env` wholesale, detaching env's `%cache` Arc from the proto body's local-slot Arc
(strong_count→1). `try_fast_hash_element_assign` (vm_var_assign_ops.rs:2606) does not update the local slot when
strong_count==1 → slot stale → `sync_env_from_locals` clobbers env with the stale slot → the state persistence saves an
empty hash. Fix = at the tail of the fast hash assign, mirror the env value to the local slot when it exists, even at
strong_count==1. Details = the proto.t row of §7.2a. pin=`t/proto-state-cache-writeback-coherence.t`.

### 7.3 ★Mandatory procedure for each slice (lessons from slice 1)

1. **Keep boxing gated by `cell_boxing_active()`** (default is reconcile, byte-identical). Cell-ifying a new surface
   = adding an implementation that boxes that cluster's vars under the toggle.
2. **Always run `make roast` locally (or the relevant synopsis)**. `make test` alone misses roast regressions
   (slice 1 could not detect let.t/code.t via make test).
3. **Expect latent bugs exposed by cell-ification** (cases that accidentally PASSed because reconcile dropped a write in a
   carrier). If a fail remains under the toggle, that is "the next real bug to fix" = record it and order it.
4. Confirm the pins PASS with the toggle both OFF and ON.

### 7.4 Final (env_dirty removal)

Once the remaining surface (toggle OFF survey) reaches 0 → hollow out `blanket_reconcile_if_dirty` →
remove `env_dirty` / `ensure_locals_synced` / `saved_env_dirty` (PLAN.md §2-E). The `pairs`/`slip` carrier-drop is made safe
at the same time. At this point, remove the `cell_boxing_active()` gate and make boxing permanently ON.

---

## 8. Hazard checklist

- [ ] **Saved-frame propagation is mandatory** (§3 lesson). Slot+env alone means the cell rolls back on method return.
- [ ] **Do not box scalars with type/where constraints** (bypasses constraint re-checks).
- [ ] **Do not box non-scalar Value kinds (Package/Sub/Instance/Proxy)** (breaks identity/meta; keep the existing skip).
- [ ] **Ordering of forward-declared subs** (for Option B). Confirm that a Nil cell box → SetLocal transparency absorbs it.
- [ ] **perf**: every access to affected vars takes a Mutex lock. Escape-aware, limited to captured-mutated. Check timed roast / fib and method-call.
- [ ] **Container decont leaks** (slice 2): audit that the cell does not leak through slice/`.kv`/`.pairs`/native raw-items reads.
- [ ] **Cross-thread** (Track C): consistency of the `Arc<Mutex>` cell with `shared_vars`/`clone_for_thread`.

---

## 9. Getting-started procedure (quick start for the next session)

**★Pure writeback coherence completed with slice 1.20 (#3400). True lazification of lazy-lists.t 24-26 also complete
(2026-06-22) = the deterministic surface of the OFF roast survey is exhausted down to only IO-Socket-Async.t's flakiness.**
With this, all the prerequisites for env_dirty removal (§7.4) are in place.

**Resolution of lazy-lists.t 24-26 (2026-06-22, complete)**:
- True cause: `.kv`/`.pairs`/`.antipairs` **eagerly forced** `make-lazy-list = gather { take ...; $was-lazy = 0 }.lazy`,
  running `$was-lazy = 0`. ON matched raku by accident via write-loss; OFF propagated correctly and exposed it (a laziness
  bug, not writeback).
- Fix: added `index_transform: Option<IndexTransform>` (Pairs/AntiPairs/Kv) to `MapGrepSpec` and made `.kv`/`.pairs`/`.antipairs`
  **lazy index-pipes** (`LazyList::new_index_pipe`). For genuinely-lazy sources (`.lazy` gather, infinite spec) they return a
  lazy pipe rather than eagerly forcing. Uses `source_idx` as the positional key; pull reuses the existing
  `pull_source_element` (incrementally consumes the gather coroutine). `force_lazy_pipe` bypasses func/grep when
  `index_transform` is Some and emits the index transformation. Early dispatch was added to all 3 dispatch paths
  (CallMethodMut fast-path, CallMethod non-mut, runtime slow-path methods.rs).
  The index-pipe carries the preserve marker, so `my @res = one.kv` stays lazy. Values are byte-identical with the eager
  version. pin=`t/lazy-pairs-kv-antipairs.t` (18; PASSES both ON/OFF).

**Next = §7.4 (physical removal of env_dirty)**: hollow out `blanket_reconcile_if_dirty` → remove
`env_dirty`/`ensure_locals_synced`/`saved_env_dirty` → remove the `cell_boxing_active()` gate and make boxing permanently ON.
IO-Socket-Async's flakiness is verified against actual behavior during this hollowing-out.

---

## 10. env_dirty physical-removal substrate grind (started 2026-06-22)

§7.4 said "hollowing out blanket reconcile lets us delete env_dirty", but **session 49's experiments showed that is
insufficient**. env_dirty has two roles:

1. Gate for **blanket reconcile** (`blanket_reconcile_if_dirty`, the env_dirty-gated O(all locals) rollback).
   Already disabled under `MUTSU_NO_BLANKET_RECONCILE` (boxing) = **removable**.
2. Perf gate for **precise reconcile** (`reconcile_locals_from_env_at_site`, pulling all locals from env at sites such as
   the carrier fallback `lives-ok{}` and Proxy STORE). **Load-bearing even under boxing**.

**Demonstration**: with `MUTSU_NO_BLANKET_RECONCILE=1` still set, making `reconcile_locals_from_env_at_site` a no-op makes
`make test` FAIL ("wrapper scalar mutation of captured var propagates" and others). ∴ Full env_dirty removal presupposes a
multi-session grind that **also makes precise reconcile unnecessary = folding each surface that depends on precise reconcile
into precise writeback / cell sharing** (§4-A's outer cell sharing, PLAN §2-E).

### 10.1 Measurement harness `MUTSU_NO_PRECISE_RECONCILE`

An independent toggle that makes `reconcile_locals_from_env_at_site` a no-op (`precise_reconcile_disabled()`,
`vm/vm_env_helpers.rs`). `MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1` (double-OFF) =
measures **boxing only = the state reached after env_dirty removal**. The fails remaining under double-OFF are "by-name
writers not yet converted to precise writeback / cell sharing". Once these reach 0 (excluding flaky), precise reconcile +
env_dirty can be physically removed.
(A harness of the same shape as the old `MUTSU_NO_BLANKET_RECONCILE` that drove the OFF survey of slices 1–1.20.)

### 10.2 double-OFF baseline (2026-06-22, 16 → 15)

t/ files failing under double-OFF (= surfaces depending on precise reconcile):
`closure-nested-writeback` (method-captured `$output`) / `concurrent-cell-writeback-coherence` /
`done-paren-stmt-modifier` (react done()) / `eval-carrier-precise-writeback` /
`junction-invocant-autothread-writeback-coherence` / `let-temp` + `let-temp-restore-writeback-coherence` /
`note-gist-and-dynamic-handle` / `react-do-whenever-tap-coherence` / `react-whenever-last-next` /
`resumable-control-signal-indirect-call` / `single-store-slice-c-prime` / `supply-on-demand-closing` /
`supply-sync-infinite-emit` / `wrap-closure-capture`. Categories = carrier / concurrency (supply/react) / closure / other.

### 10.3 Slice S1 (2026-06-22) — make bound-Proxy substr-rw/subbuf-rw/undefine precise (16→15)

The Proxy STORE of `my $r := substr-rw($s, ...); $r = v` writes `$s` by name to env, but STORE runs one frame below the
slot owner, so slice 1.13's `pending_rw_writeback_sources` (drop-on-miss) loses it.
**Fix**: at STORE's recording sites (the substr-rw/subbuf-rw/undefine branches of `builtins_lvalue.rs`), also record into the
retain-on-miss `pending_caller_var_writeback` (new helper `record_caller_var_writeback`), and at the Proxy STORE assign sites
(3 local sites in `vm_var_assign_ops.rs` + the global by-name site in `vm_misc_ops.rs`) drain precisely via
`apply_pending_rw_writeback` instead of `reconcile_locals_from_env_at_site`. Retain-on-miss carries it even when the owner
slot is several frames up (same shape as slice 1.18). Blanket reconcile is left at the local sites as a fallback for
arbitrary user `Proxy` STOREs (a no-op under the double-OFF harness) = the default build is byte-identical. pin = existing
`t/substr-rw-lvalue-writeback-coherence.t` (test 4; now PASSES under double-OFF). make test 9890 (PASSES both default/double-OFF).

### 10.4 Slice S2 (2026-06-22) — make `let`/`temp` restore precise (15→13)

The block-exit restore of `temp $x = 20` only does `env.insert` on the non-cell path of `restore_let_value` (`accessors.rs`)
= slot stale (`$x` stays 20 under double-OFF). The restore knows the restored name exactly. **Fix**: on the non-cell path,
record the restored name into `pending_rw_writeback_sources` (drop-on-miss, for same-frame bare blocks) plus
`pending_caller_var_writeback` (retain-on-miss, for `temp` in nested callees), and at the 3 block-exit op sites
(`vm_misc_ops.rs` success/err, `vm_control_ops.rs` err) drain precisely via `apply_pending_rw_writeback` before
`reconcile_locals_from_env_at_site`. Blanket reconcile is left as a fallback (a no-op under the harness) = default-build
behavior unchanged. Cell-boxed bindings are unaffected since the slot sees them via the shared Arc as before.
pin=`t/let-temp.t` + `t/let-temp-restore-writeback-coherence.t` (now PASS under double-OFF). make test 9904.

### 10.5 Slice S3 (2026-06-22) — make closure/method nested-capture writeback precise (13→10)

`cap({ note "" but role { method gist { $seen = 1 } } })` (note-gist) / `capture-out`'s `$output`
(closure-nested-writeback) / `&f.wrap(-> { $seen = True; callsame })` (wrap-closure-capture) = a nested
gist/method/wrapper closure mutates a captured outer lexical, but that lexical is **not a direct free var** of the closure
in question (captured within the nesting), so the free_var recording of §closure-dispatch (line 811) cannot pick it up.
**Fix**: (1) in the env-scan writeback loop of closure dispatch (`vm_closure_dispatch.rs`), when the value of a
caller-visible var being written back has changed, record it into `pending_caller_var_writeback` (retain-on-miss)
(guarded by `cell_boxing_active()` = not allowed on the default build's hot closure path; blanket reconcile carries it there).
(2) at wrap dispatch (`vm_call_func_ops.rs:518`, `vm_call_exec_ops.rs:56`), add `apply_pending_rw_writeback` (drains the
wrapper's recordings). Blanket reconcile is left as a fallback.
pin=`t/note-gist-and-dynamic-handle.t` + `t/closure-nested-writeback.t` + `t/wrap-closure-capture.t` (now PASS under
double-OFF). make test 9904.

### 10.6 Slice S4 (2026-06-22) — make cross-frame caller writeback of regex embedded `{ }` / `:let` precise (10→8)

`sub do-match($txt) { $txt ~~ / (\d+) { $tracked = +$0 } / }` (single-store-slice-c-prime test 7) /
`my regex la { :let $a = 5; … }` inside EVAL (eval-carrier-precise-writeback test 12) = a regex's embedded `{ }` /
`:let` block writes by name to `env` a caller lexical (`$tracked`/`$a` of the sub's caller / the EVAL's caller) that is
**not** a slot of the frame where the match runs (`do-match`'s body / the EVAL'd code). `writeback_match_locals`
(at the match site, `vm_comparison_ops.rs`) **only writes the current frame's slots**, so the owning slot is one or more
frames up and stays stale (`$tracked=-1`/`$a=1` under double-OFF). EVAL's cell-boxing (slice 1.7) picks up `$x=5`-style
`SetGlobal` writes via the cell, but `:let` severs the cell link via `restore_env_entries`' direct env insert, so it cannot.
**Fix**: at the match site, after `writeback_match_locals`, record the embedded write names (`pending_local_updates`)
that are neither match specials (`$/`/`$0`…) nor current-frame slots into the retain-on-miss
`pending_caller_var_writeback` (`record_caller_var_writeback`). The `apply_pending_caller_var_writeback` at the call site
returning to the owning frame drains it (same shape as slices 1.18/S1-S3). **1 fix clears 2 surfaces** (both the sub carrier
and the EVAL carrier). Guarded by `cell_boxing_active()` = the default build is carried by blanket/precise reconcile as
before = byte-identical. pin=`t/single-store-slice-c-prime.t` (test 7) + `t/eval-carrier-precise-writeback.t` (test 12)
(now PASS under double-OFF).

### 10.7 Slice S5 (2026-06-22) — make per-eigenstate writeback of junction invocant autothreading precise (8→7)

`my Mu $x = JB1.new | JB1.new | JB2.new; $x.a` (`method a { $cnt1++ }` / JB2 does `$cnt2++`) = autothreading an invocant
junction over a user method, with each eigenstate mutating captured-outer / `our` variables. Each eigenstate's dispatch
records its by-name write into `pending_rw_writeback_sources`, but **the next eigenstate's dispatch overwrites that buffer**,
so all that remains for the post-loop drain is **only the last eigenstate's sources**. Variables written only by EARLIER
eigenstates (`$cnt1` written while the last wrote `$cnt2`) are lost and the owning slot goes stale (double-OFF gives
`$cnt1=1`, expected 2). When all eigenstates write the same variable, the last source suffices, so it worked (the exposure is
only "different eigenstates write different variables").
**Fix**: after each eigenstate call in the junction loop, drain `pending_rw_writeback_sources` + `pending_caller_var_writeback`
and accumulate into the retain-on-miss `pending_caller_var_writeback` (`record_caller_var_writeback`, dedup). A single
post-loop `apply_pending_caller_var_writeback` writes the owning caller slots precisely from env (which already holds the
accumulated values of all eigenstates). Applied symmetrically to both junction paths, CallMethod (non-mut) / CallMethodMut
(mut) (`$cnt++` goes via the mut path). Guarded by `cell_boxing_active()` = the default build keeps the blanket
`reconcile_locals_from_env_at_site` pull as before = byte-identical.
pin=`t/junction-invocant-autothread-writeback-coherence.t` (6; now PASSES under double-OFF). The regression source
`roast/S03-junctions/autothreading.t` PASSES 107/107.

### 10.8 Slice S6 (2026-06-22) — make resumable CONTROL handler writeback precise (7→6)

`my $out=''; CONTROL { default { $out ~= "[{.message}]"; .resume } }; my $w = &warn; $w.("indirect")` =
a resumable `warn` raised from an indirect call (`$w.(...)`/`&warn.(...)` = CallOnValue) is executed inline by the enclosing
CONTROL's `resume_safe` handler (`try_resume_safe_control_inline`, builtins_control_flow.rs). The handler body mutates the
installing frame's lexical `$out`, but the handler only reconstructs locals from env → runs → **flushes changed slots to env
+ env_dirty**; the frame's local SLOT is updated only by the warn-call site's blanket/precise
`reconcile_locals_from_env_at_site` = a no-op under double-OFF. **Direct `warn` (ExecCall) passes**; only indirect
(CallOnValue) dropped (measured: after the indirect call, `env["out"]=[indirect]` survives, slot[0] is stale, and the read
sees the slot and comes up empty). **Fix**: in the handler's flush loop, record changed names into
`pending_rw_writeback_sources` (drop-on-miss, same-frame) plus `pending_caller_var_writeback` (retain-on-miss; a deeper raise
site carries it up to the installing frame). The **ungated `apply_pending_rw_writeback`** that both call sites
(ExecCall/CallOnValue) already run drains env → slot. Guarded by `cell_boxing_active()` = the default build is carried by
blanket reconcile as before = byte-identical. pin=`t/resumable-control-signal-indirect-call.t` (5; now PASSES under double-OFF).

### 10.9 Slice S7 (2026-06-22) — make react/whenever captured-outer writeback precise (6→0)

The `whenever` callback of `react whenever Supply.from-list(…) { …; $i++ }` mutates the captured-outer lexical `$i`.
The callback writes env by name via `vm_call_map_block` without per-write recording, and only the post-loop
`reconcile_locals_from_env_at_site` of `exec_react_scope_op` (vm_register_ops.rs) (a no-op under double-OFF) delivers it to
the slot. **Fix**: **snapshot the caller-frame slot-backing env values before and after the react event loop** and write back
precisely only the changed slots from env (the only precise-ification method for by-name writers without per-write
recording = identify the names the loop wrote by diffing). **This 1 fix sweeps 5 surfaces**
(`done-paren-stmt-modifier`/`concurrent-cell-writeback-coherence`/`react-whenever-last-next`/
`supply-on-demand-closing`/`supply-sync-infinite-emit`). Additionally, the `my $tap = do whenever $sup {…}` bind of
`exec_whenever_scope_op` (:1841) has a **known target_var name**, so write back precisely only that slot
(`react-do-whenever-tap-coherence`). Synchronous `from-list` emit = single-threaded so tractable. Guarded by
`cell_boxing_active()` = the default build is carried by blanket reconcile = byte-identical. pin=`t/done-paren-stmt-modifier.t` (4) +
`t/react-do-whenever-tap-coherence.t` (2) and others (all 6; now PASS under double-OFF). The broad
supply/react/concurrency/promise/start t/ also PASSES in both modes.

### 10.10 t/ surface reaches 0; **the roast double-OFF sweep exposes 25 files of hidden surface** (authoritative)

With S1–S7, **the t/ pins (16) plus the broad supply/react/concurrency/promise/start cluster all PASS in both modes
(default / double-OFF)** = the precise-reconcile-dependent surface observable in t/ is exhausted. **But the first actual
double-OFF sweep of the full roast whitelist (1285) (`MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1` release)
showed 25 files failing deterministically = the t/ pins were incomplete** (the same lesson as session 45's discovery that
"the OFF roast survey is what is authoritative"). Physical removal of env_dirty comes only after fully clearing this roast
surface. Diagnostics in `tmp/roast-double-off.log` / `tmp/roast-double-off-fails.txt`.

**The 25 roast double-OFF files (before S8, by category)**:
- **S03 operators (andthen/orelse/notandthen)** = captured-outer writes of a user `method defined { $calls++ }`.
  → **✅ Resolved by S10.11** (CallDefined snapshot writeback; 1 fix, 3 files).
- **S14 roles (anonymous/mixin-6e/parameterized-mixin/rw/submethods-6e, 5)** = writeback of role mixin/`rw` accessors
  (anonymous/parameterized-mixin are abort=Bad plan).
- **S02 types (baghash/mixhash/set, 3)** = captured writes of Set/Bag/Mix operations.
- **S02 names (our/symbolic-deref, 2)** = by-name writeback of `our`/symbolic deref.
- **Others**: S02-types/lazy-lists, S04 gather (abort), S04 terminator, S04 pointy-rw, S06 lvalue-subroutines,
  S06 named-parameters, S12 coercion-methods, S12 primitives, S12 defer-next, S17 cas-loop, S17 throttle, S32 kv.

**∴ Physical removal of env_dirty (§7.4 / PLAN §2-E) requires roast double-OFF 0**: (1) clear roast 25→0 in slices →
(2) hollow out `blanket_reconcile_if_dirty` / `reconcile_locals_from_env_at_site` → (3) remove `env_dirty` /
`ensure_locals_synced` / `saved_env_dirty` → (4) remove the `cell_boxing_active()` gate and make boxing permanently ON
(= turning it into a derived view of a single store).

### 10.11 Slice S8 (2026-06-22) — make user `.defined` (andthen/orelse/notandthen) writeback precise (roast 25→22)

`my $calls=0; my class Foo { method defined { $calls++; True } }; Foo andthen meow $_` = `andthen`/`orelse`/`notandthen`
determine the LHS's definedness via `OpCode::CallDefined`, dispatching to a user `method defined` if present (vm.rs:2831).
That user method writes the captured-outer `$calls` by name to env, but it only reaches the slot via `CallDefined`'s
post-call `reconcile_locals_from_env_at_site` (a no-op under double-OFF) and goes stale (double-OFF gives `$calls=0`,
expected 1). It goes via `run_instance_method`, so there is no per-write recording → **snapshot the caller-frame
slot-backing env values before and after the user-method call** and write back precisely only the changed slots (same
technique as react S7). Guarded by `cell_boxing_active()` = the default is carried by blanket reconcile = byte-identical.
**1 fix clears 3 roast files (S03 andthen/orelse/notandthen)**. pin=roast/S03-operators/{andthen,orelse,notandthen}.t
(now PASS under double-OFF).

### 10.12 Slice S9 (2026-06-22) — make carrier writeback of symbolic-deref stores precise (roast 22→21)

`my $a_var=42; my $b_var="a_var"; lives-ok { $::($b_var) = 23 }; is $a_var, 23` = `$::($name) = v` (symbolic deref store,
`exec_symbolic_deref_store_op`) and `::('$x') = v` (indirect type lookup store, `exec_indirect_type_lookup_store_op`) write
the target lexical by name to env and update only **the current frame's** slot via `update_local_if_exists`. Inside a
`lives-ok { }` carrier the target slot is in the carrier-caller's frame, reachable only via the carrier writeback
(`writeback_carrier_writes`), but neither store op **logged to carrier_writes**, so OFF was never reconciled and stale
(works in a bare block but fails only inside lives-ok). **Fix**: after the env insert in both store ops, call
`note_caller_env_write(&store_name)` to log into carrier_writes + env_dirty (the same pattern as regex `:let` and `s///`
writeback). Scalar, so no `:=` cell hazard. The default build is byte-identical (scalar reconcile is a subset of blanket).
pin=roast/S02-names/symbolic-deref.t (now PASSES under double-OFF). **21 roast double-OFF remaining** (our.t is a separate
mechanism, EVAL+class+`$GLOBAL::`++, not yet cleared).

### 10.13 Slice S10 (2026-06-22) — make captured-outer writeback of user Proxy STORE precise (roast 21→20)

`my $realvar="foo"; sub proxyvar($p) is rw { Proxy.new(STORE => method ($v) { $realvar = $v }, …) }; proxyvar("PRE")="BAR";
is $realvar, 'BAR'` = the STORE method of a user `Proxy` returned by an lvalue sub writes the captured-outer scalar
`$realvar` by name (`assign_proxy_lvalue`, builtins_lvalue.rs). The target slot is in the assign-caller's frame and only
reachable via the assign site's blanket reconcile (a no-op under double-OFF), so stale (double-OFF gives `foo`, expected
`BAR`). **Fix**: **snapshot only the writeback-safe scalars of env before and after** `assign_proxy_lvalue`'s STORE call,
record the changed names into `record_caller_var_writeback` (retain-on-miss) → the assign call site's
`apply_pending_caller_var_writeback` drains. Scalar-limited (the `is_writeback_safe_scalar` filter), so no container/`:=`
cell hazard. Guarded by `cell_boxing_active()` = the default is carried by blanket reconcile = byte-identical.
pin=roast/S06-routine-modifiers/lvalue-subroutines.t (now PASSES under double-OFF).
**20 roast double-OFF remaining**.

### 10.14 Slice S11 (2026-06-22) — make Set/Bag/Mix writeback of lives-ok container carriers precise (roast 20→17)

`my $b=(…).BagHash; lives-ok { $b<a> = 42 }; is $b<a>, 42` = a block Test fn (`lives-ok`/`dies-ok` = the
`exec_call_pairs_op` carrier) mutates a captured-outer Set/Bag/Mix via env, but `writeback_carrier_writes` protectively
skips container slots (leaving it to the barrier), so double-OFF is stale. **Fix**: **snapshot env before and after** carrier
execution and write through the changed slots. **Limit the targets to COW aggregates (scalar + Set/Bag/Mix)** =
`Value::Set/Bag/Mix(Arc<…Data>)` COWs on in-place mutation via `make_mut` and detaches to a new Arc (the pre snapshot keeps
the old Arc), so diff detection is reliable. **Array/Hash excluded** (hazard: env being a COW-detached copy would destroy
interior `:=` element cells). **★Instance also excluded** = attributes are a **shared cell** of `Arc<RwLock>` mutated in
place, so the snapshot shares the cell and pre==post makes diff detection **impossible** (Instance rw-accessors are a
separate slice = left in S14-roles/rw.t). Guarded by `cell_boxing_active()` = the default is byte-identical.
Regression check: element-bind-cell/S03-binding nested, arrays/set, mix all PASS (no `:=` hazard). pin=roast/S02-types/{baghash,
mixhash,set}.t (now PASS under double-OFF). **17 roast double-OFF remaining** (including rw.t Instance rw-accessor).

### 10.15 Slice S12 (2026-06-22) — extend lives-ok carrier writeback eligibility to slot-overwritable (roast 17→11)

S11 (§10.14) limited carrier writeback to **COW aggregates (scalar + Set/Bag/Mix)**, but since it judged by "the type of the
new env value" it missed many cases. S12 switches eligibility to **"the type of the *current value* of the slot being
overwritten"** (`slot_carrier_overwritable`): the exclusions are **only 2 kinds** — `HashSlotRef`/`ContainerRef`
(`:=` binding cells) and plain `Array`/`Hash` (hazard: an env COW-detached copy destroys interior `:=` element cells) —
and everything else (scalar/Set/Bag/Mix/Mixin/Instance/…) is write-through eligible. This also picks up
`lives-ok { $a does Role[42] }` (Int(0) slot → Mixin, a type change) / `lives-ok { $obj.r1++ }` (Instance rw-accessor,
diverged Instance) / Pair `.kv`/`.values` rw aliasing / `$GLOBAL::`++. In-place attribute mutation of an Instance becomes
pre==post via the shared `Arc<RwLock>` cell and the diff does not fire = harmless (only genuine value/type changes are
written through). **The full roast double-OFF sweep goes 17→11 (zero new regressions; all 11 remaining are pre-existing)
= 6 more files cleared vs S11** (S02-names/our, S04 pointy-rw, S04 gather, S12 coercion-methods, S14 rw, S32 kv).
Guarded by `cell_boxing_active()` = the default is byte-identical. Regression check: S03-binding nested/arrays,
element-bind-cell all PASS. **11 roast double-OFF remaining**: S14 `does`-mixin block-scoped reassignment 4 (anonymous/
mixin-6e/parameterized-mixin/submethods-6e = `$a does R` reassignment inside a block does not reach the outer env; next
slice) / lazy-lists / terminator / named-parameters / primitives / defer-next / cas-loop / throttle.

### 10.16 Slice S13 (2026-06-22) — make captured-outer writeback of does/but mixins precise (roast 11→7)

The 4 S14-roles `does`/`but` mixin files (anonymous/mixin-6e/parameterized-mixin/submethods-6e). 3 distinct misses:
1. **`Mixin` PartialEq delegating to the inner value**: `lives-ok { $a does Role[42] }` turns the Int(0) slot into a Mixin,
   but since `Value::Mixin(Int(0),_) == Int(0)` (PartialEq delegates to the inner, value/mod.rs:2881), the carrier's diff
   (`pre != cur`) is false → the overwrite is missed. **Fix**: add a `std::mem::discriminant` comparison to the diff check in
   `carrier_writeback_changed_aggregates` (detects the variant change Int→Mixin even under value equality).
2. **The `does`/`but` ops themselves depending on reconcile**: the role's `submethod TWEAK`/`BUILD` run by
   `$y does R` (`exec_does_op`/`exec_does_var_op`) / `$obj but R` (`exec_but_mixin_op`) mutates a captured-outer counter
   (`my $n=0; submethod TWEAK { $n++ }`), but each op's post-call `reconcile_locals_from_env_at_site` is a no-op under
   double-OFF. **Fix**: add a carrier snapshot diff (`snapshot_carrier_overwritable_env` +
   `carrier_writeback_changed_aggregates`) to the 3 ops, guarded by `cell_boxing_active()`.
3. **Type change of a Hash slot**: `my $a = {:x}; lives-ok { $a does role {...} }` turns a **Hash** slot into a Mixin. Hash
   is normally excluded due to the interior `:=` element cell hazard, but **a type change (whole replacement Hash→Mixin) is
   safe**. **Fix**: in `carrier_writeback_changed_aggregates`, also overwrite Array/Hash slots but only when
   `discriminant(slot) != discriminant(env)` (type change) (same-variant detached copies continue to be skipped).
Guarded by `cell_boxing_active()` = the default is byte-identical. **The full roast double-OFF sweep goes 11→7 (zero new
regressions)**. Regression check: S03-binding nested/arrays PASS. **7 roast double-OFF remaining**: lazy-lists / terminator /
named-parameters / primitives / defer-next / cas-loop / throttle (each a separate mechanism).

### 10.17 Slice S14 (2026-06-22) — make captured-outer writeback of param `where` clauses precise (roast 7→6)

`my $t=''; sub order_test(:$a where { $t ~= 'a' }, :$b where { $t ~= 'b' }) {…}; order_test(b=>2, a=>3); ok $t ~~ /a.*b/`
= a named param's `where { $t ~= 'a' }` clause mutates the captured-outer `$t` during binding. **Measurement confirmed
slot-only**: after `order_test(...)`, a closure-mediated read gives `$t='ab'` (reaches env) but a direct read is empty
(caller slot stale) = the where-clause's write reaches the outer env, but the caller slot is refreshed only by the
call-site's blanket pull (a no-op under double-OFF).
**Fix**: in the where-constraint named-eval (`types/binding.rs` ~797), **snapshot env's writeback-safe scalars before and
after the eval** and record the changed names (excluding `_`/the param itself) into the retain-on-miss
`pending_caller_var_writeback` (`record_caller_var_writeback`) → the compiled-function call site's
`drain_and_reconcile_after_cached_call` → `apply_pending_caller_var_writeback` drains. Scalar-limited
(`is_writeback_safe_scalar`), so no `:=` hazard. Guarded by `cell_boxing_active()` = the default is byte-identical
(where-constraints are a rare, non-hot path). pin=roast/S06-signature/named-parameters.t (now PASSES under double-OFF).
**6 roast double-OFF remaining**: cas-loop / defer-next / primitives / lazy-lists (laziness, separate axis) /
throttle (timing) / terminator (parser).

### 10.18 Slice S15 (2026-06-22) — make captured-outer writeback of CAS blocks precise (roast 6→5)

`my $was='WRONG'; cas($head, { $was = $_; $next }); ok $was === Node` = the block of `cas $var, { … }` mutates the
captured-outer caller scalar `$was`. **Confirmed slot-only** (a closure-mediated read is correct, a direct read is stale;
manifests only inside a bare block): the block's write reaches env, but the caller slot is refreshed only by the blanket pull
at cas's call site (a no-op under double-OFF). **Fix**: snapshot env's writeback-safe scalars before and after the block
execution (`call_sub_value`) in `builtin_cas_var` (`builtins_atomic.rs`), and record the changed names (excluding
`_`/`$_`/the cas target name) into the retain-on-miss `pending_caller_var_writeback` → the cas call site's
`apply_pending_rw_writeback` (slow path L600) drains. Scalar-limited, so no `:=` hazard. Guarded by `cell_boxing_active()`
= the default is byte-identical. pin=roast/S17-lowlevel/cas-loop.t (now PASSES under double-OFF).
**5 roast double-OFF remaining**: defer-next (multi-dispatch) / primitives (meta compose) / lazy-lists (laziness, separate
axis, not writeback) / throttle (timing, flaky family) / terminator (auto-curly array composer, parser).

### 10.20 Slice S16 (#3437, 2026-06-22) — make captured-outer writeback of proto-multi candidates precise (roast 5→4)

The candidate body of `proto method l(|){*}` + `multi method l(%t,*@l){ $r ~= '%'; &?ROUTINE.dispatcher()(self,…) }` mutates
the captured-outer caller scalar `$r`. **True cause = proto candidates always go via the slow path**: `proto method`'s `{*}`
(`call_proto_dispatch`, with method_ctx) re-dispatches the candidate via `call_method_with_values`, and the candidate goes
through `run_instance_method_resolved` (class.rs:934) rather than the compiled path (`vm_call_method_compiled` = which
records env_dirty/saved_env_dirty/pending). This slow path propagates captured scalar writes to env via `merged_env`
(the "propagate only keys present in saved_env" loop at L1421) but **sets no `env_dirty` and records no precise writeback**.
∴ The caller's **local slot** is refreshed only by the call-site's blanket pull, and moreover, since the slow path does not
set `env_dirty`, the blanket pull never even fires and **the caller slot is stale even in the default build** (a latent bug,
not double-OFF-only). Measurement = a MUTSU_DBG probe confirmed `merged_env["r"]=="X"` (env propagated) but the top-level
slot empty.
**Fix**: in `try_proto_method_body` (dispatch.rs), **snapshot env's writeback-safe scalars before and after**
`run_proto_method`, and record the changed names (excluding `_`/`$_`) into the retain-on-miss
`pending_caller_var_writeback` (`record_caller_var_writeback`) → the `apply_pending_rw_writeback` at the proto call site
(right before the `try_proto_method_body` short-circuit return in `vm_call_method_ops`/`vm_call_method_mut_ops`) drains.
**★Ungated** (no `cell_boxing_active()` guard) = precise writeback is a subset of blanket reconcile and does not change
correct results, and it also fixes the default build's latent bug (stale on a `say "$r"` read). An `is $r` read steps on a
reconcile site, so roast defer-next.t happened to pass by accident in default, but a direct `say` read was broken even in
default. pin=`t/proto-multi-captured-writeback-coherence.t` (5; PASSES both ON/OFF). Scalar-limited
(`is_writeback_safe_scalar`), so no `:=` hazard. make test 10015.
**4 roast double-OFF remaining**: primitives (meta compose, writeback candidate) / lazy-lists (laziness, separate axis) /
throttle (timing, flaky family) / terminator (auto-curly array composer, parser).

### 10.21 Slice S17 (#3438, 2026-06-22) — make captured-outer writeback of custom HOW type-check methods precise (roast 4→3)

`class UnionTypeHOW { method type_check(Mu $, Mu \c){ ++$union-type-checks; … } method find_method(Mu $,$n){ $find++; … } }`
plus `Int ~~ $union` / `$union ~~ Int` smartmatches against a custom type from `Metamodel::Primitives.create_type($how, …)`.
The HOW's `type_check`/`accepts_type`/`find_method` methods `++` a captured-outer caller **counter**. **Same family as
S16**: HOW methods are dispatched at type-check time via the slow path (`run_instance_method_resolved`), propagating
captured scalar writes to env but setting no `env_dirty` and recording no precise writeback → the caller slot is refreshed
only by the blanket pull. **The counter being a read-modify-write (`++$c`) makes it even nastier**: if the slot is stale,
the next smartmatch statement re-flushes the stale slot to env, and the increment is lost across statements (under
double-OFF, find=4→1 and checks=2→0 collapse). **Fix**: replace the 3 dispatch sites (`accepts_type`/`find_method` in
`metamodel.rs`, `type_check` in `smart_match.rs`) with a new helper `call_how_method_recording_writeback` (snapshots env's
writeback-safe scalars before and after the HOW call → records the changed names into `pending_caller_var_writeback`) →
the `apply_pending_caller_var_writeback` at the smartmatch op (tail of `exec_smart_match_expr_op`) drains (slot refresh at
the end of each statement = the next statement's re-flush uses fresh values). **★Ungated** (like S16; scope limited to
custom-HOW dispatch; precise writeback is a subset of blanket; also fixes the default build's latent bug in bare
sink-context smartmatches `$obj~~T;$obj~~U;`). pin=`t/custom-how-type-check-writeback-coherence.t` (6; PASSES both ON/OFF).
**3 roast double-OFF remaining**: lazy-lists (laziness, separate axis) / throttle (timing, flaky family) /
terminator (auto-curly composer, parser).

### 10.22 Slice S18 (#3439, 2026-06-22) — apply the EVAL carrier's scalar-reassignment writeback to container slots too (roast 3→2)

`my $z = []; EVAL q'$z = do { 1 } + 2;'; is $z, 1` = the EVAL carrier scalar-reassigns the captured-outer caller scalar
`$z` **into a slot that previously held a container**. **True cause = the reconcile-eligibility check of
`writeback_carrier_writes` (vm_env_helpers.rs) was "is the old slot value a scalar"**: `my $z = []` (slot=Array) → on
writeback of EVAL's `$z = 1` (env=Int), the slot's current value is an Array so the scalar branch is skipped → the container
protection branch (which refuses the overwrite to avoid the COW `:=` cell hazard) → falls back on blanket reconcile (a no-op
under double-OFF) and the slot stays a stale Array. **This case was mistakenly classified as "parser (auto-curly)", but the
substance is EVAL carrier scalar writeback** (normal gives z=1; the auto-curly parse is fine; only double-OFF gives z=[]).
**Fix**: change the eligibility check to **whether the new env value is a scalar** (`is_writeback_safe_scalar(&env_val)`
plus the slot not being a `:=` bind cell = `ContainerRef`/`HashSlotRef`). A scalar new value carries no container COW-copy
hazard, so it can safely overwrite even a slot that previously held a container. `my @e = …; EVAL q'@e = 7,8'`
(container→container) has a container new value so it keeps the old protection branch = unaffected. The `:=` bind cells
(`t/element-bind-cell.t`, `S03-binding/nested.t`, `arrays.t`) PASS in both modes, no regressions.
pin=`t/eval-carrier-scalar-writeback-coherence.t` (6; PASSES both ON/OFF). **This fix is general** (rescues any
`EVAL q'$x = scalar'` where the slot held a container).
**2 roast double-OFF remaining**: lazy-lists (laziness, separate axis) / throttle (timing, flaky family).

### 10.23 S19 — true lazification of lazy-lists (gather grep/map; PR in progress, 2026-06-22)

The `S02-types/lazy-lists.t` 14/16 (`grep is lazy` / `map is lazy`) that §10.19 identified as the "true laziness blocker"
is **resolved by true lazification** (27/27 PASS even under double-OFF). **True cause = blanket reconcile was not
load-bearing after all**: the gather was eagerly forced in **both** normal and double-OFF (the §10.19 "take counter=10"
measurement was correct but the conclusion was inverted). Normal showed `$was-lazy=1` only because the captured write of the
gather tail `$was-lazy=0` was **being write-lost** (ON matched raku by accident via write-loss = doc §7.3 lesson #3).
∴ Blanket reconcile was not maintaining laziness — the bug was exposed the moment reconcile propagated the write. The fix is
not writeback but **pulling `gather { … }.grep/.map` through a lazy pipe instead of eagerly forcing it**.

The true eager-force sources were **4 sites** (all with a "make `.lazy` gather/infinite specs into lazy pipes for map/grep"
exception that was only `lazy_pipe.is_some() || is_infinite_spec()`, thereby **excluding gather coroutines**):
1. `is_lazy_pipe_source` (methods_collection.rs:69) — did not recognize gather as a lazy pipe source → OR in
   `ll.needs_vm_lazy_dispatch()` (`is_from_gather() || is_infinite_spec()`).
2. `try_native_method` (vm_native_dispatch.rs:41) — routed gather+map/grep to the native impl and materialized → unified on
   the `is_lazy_pipe_source` gate to defer.
3. `vm_call_method_ops.rs:733` / 4. `vm_call_method_mut_ops.rs:474` (the force block before method dispatch) / plus
   `runtime/methods.rs:3157` (the slow-path force) — add `|| ll.is_from_gather()` to the map/grep exception so gather is not forced.
The infrastructure (`pull_source_element`'s `Value::LazyList → force_lazy_list_vm_n(ll, idx+1)`, vm_helpers.rs:1131) already
exists = coroutines can be resumed one take at a time. Plain gather (non-`.lazy`) is also lazy in Rakudo under grep+slice,
so gate on `is_from_gather()` (the lazy marker is lost on sub return = `is_genuinely_lazy()` won't do).
pin=`t/lazy-gather-grep-map-laziness.t` (8; PASSES in both modes).

**★Remaining double-OFF surface (authoritative survey, all 1285 release, after S19) = 2 (zero writeback candidates, zero new regressions)**:
- **`S04-statements/lazy.t`** = **resolved by S20** (`Value::LazyThunk` self-comparison deadlock; see §10.24 below).
- **`S17-supply/throttle.t`** 3,4 = timing flaky (as in §10.19; if no deterministic pin is possible, drop it from the removal blockers).
(The spurt.t / gb18030, gb2312, shiftjis-encode-decode.t hits in the survey are **harness artifacts** = raw `prove -e mutsu`
misdetects without the fudge wrapper; all PASS via `run-roast-test.sh`. spurt.t is the stale temp-file flakiness. Not
double-OFF blockers.)

**★The full-consumption-through-pipe gather tail writeback remains under double-OFF** (`my @a=gather{…;$t=1}.grep(*>1); ok $t`):
a direct gather force propagates, but via a lazy pipe (grep) the inner `force_lazy_list_vm_n`'s
`reconcile_caller_after_lazy_force` drains drop-on-miss into the wrong frame mid-pipe and loses it.
**lazy-lists.t (which slices, never fully consuming) does not step on it**, so it was left unfixed in S19 (also excluded
from the pin). A writeback slice candidate before env_dirty removal (retain-on-miss conversion or draining at the outer force).

### 10.24 S20 — LazyThunk self-comparison deadlock (`use Test` END phaser; PR, 2026-06-22)

Resolved the double-OFF **hang** of `S04-statements/lazy.t`, which §10.23 called "the next most likely double-OFF blocker".
**A futex deadlock, not a busy loop** (confirmed by `ps` STAT=S, %CPU=0, WCHAN=`futex_do_wait`, zero perf on-CPU samples).

**True cause = double-locking a non-reentrant mutex in a `Value::LazyThunk` self-comparison**. `PartialEq`
(value/mod.rs:2885) for `(LazyThunk(a), LazyThunk(b))` locks `a.cache.lock()` → then `b.cache.lock()`, but **when a and b
are the same Arc it double-locks the same mutex** and deadlocks (std `Mutex` is non-reentrant). The path that steps on it is
**the END phaser overlay of `finish()`** (run.rs:1083 `if v != orig_v`): a lexical bound with `my $x := lazy { … }` enters
**both** the END phaser's captured env (`v`) and the original env (`orig_v`) as the same Arc, making the `v != orig_v`
comparison a self-comparison. **Why double-OFF only** = under normal, reconcile forces/replaces the thunk before END or
takes another path, breaking the same-Arc identity; under double-OFF the same Arc survives to END and reaches the
self-comparison.

**Fix** = following the same pattern as `ContainerRef` (value/mod.rs:2904), **add `Arc::ptr_eq(a, b)` at the head of the
comparison to short-circuit before locking** (an identical thunk is trivially equal). A one-line general-purpose bug fix
(also plugs the latent deadlock beyond double-OFF). **Minimal reproduction** = `use Test; plan 1; my $var := lazy { 42 };
ok 1;` (no force/capture needed; reaching the END phaser is the trigger). pin=`t/lazythunk-self-compare-deadlock.t`
(4; PASSES in both modes). lazy.t 10/10 PASSES in both modes. make test 10069 PASS (zero regressions).

**★Remaining double-OFF surface (after S20) = 1 (throttle) → resolved by S21 (§10.25)**.

### 10.25 S21 — Supply `.tap` callback captured-outer writeback (PR, 2026-06-22)

Resolved `S17-supply/throttle.t`, the "1 remaining" from §10.24. **The handoff's "timing flaky" label was wrong; the
substance is a deterministic writeback bug** (tests 3-4 fail 3 runs in a row; normal is a deterministic PASS = not flaky).
The tap callback of `(1..10).Supply.throttle(1,.5).tap: { $max = $max min …; $min = $min max …; $before = now }` writes the
captured-outer scalars `$min`/`$max`/`$before`, but under double-OFF they do not propagate to the slots and stay at their
initial values (`$min=0`/`$max=10`) → `ok $min > .5`/`ok $max < .9` fail (**leftover initial values, not timing**). The
`@seen` array did propagate via Arc sharing — evidence that this is a "only scalars drop" writeback problem.

**True cause = the synchronous tap emission loop's (native_supply_mut_methods.rs:474-498) `call_sub_value(tap_cb,…)` does
not record captured writes**. The tap callback is a closure executed immediately on the main thread = the escape analysis
does not box the outer lexicals, so the slots are never refreshed.
**Fix** = after the loop, call `record_eager_block_free_var_writeback(&code, &data.params)` from the `Value::Sub`'s
`compiled_code`, recording the free-var writes into `pending_rw_writeback_sources` → the `.tap` `CallMethodMut` op drains
via `apply_pending_rw_writeback` (the same machinery as the lazy-map / gather / cross-shortcircuit carriers; the same class
as S19's S7 react). throttle.t PASSES in both modes. pin=
`t/supply-tap-callback-writeback.t` (4; PASSES in both modes; pins the writeback with a fast supply tap needing no timing). make test 10105 PASS.

**★★ roast double-OFF deterministic surface = 0 reached** (S4–S21). The remaining non-deterministic items = the
full-consumption-through-pipe gather tail writeback (§10.23; the inner force of a grep via a lazy pipe drains drop-on-miss
into the wrong frame; not hit by roast) and the IO-Socket-Async.t flakiness only.
∴ **The prerequisite for physical env_dirty removal (§7.4 / PLAN §2-E) — double-OFF roast 0 — is achieved**. Next session =
hollow out `blanket_reconcile_if_dirty` / `reconcile_locals_from_env_at_site` → physically remove `env_dirty` /
`ensure_locals_synced` / `saved_env_dirty` → remove the `cell_boxing_active()` gate and make boxing permanently ON.
**★Lesson: re-verify the handoff's "timing flaky" classifications (same shape as S18 terminator = the EVAL writeback
misclassification). Confirm determinism with 3-5 runs = throttle was a deterministic writeback bug.**

### 10.26 ★★ Boxing permanently ON — the goal "env↔locals container cell sharing" realized as the default behavior (PR, 2026-06-22)

After S4–S21 achieved roast double-OFF deterministic surface 0 (§10.25), **flipped `blanket_reconcile_disabled()` /
`precise_reconcile_disabled()` (vm_env_helpers.rs) to permanent `true`**, retiring the env→locals reconcile
(blanket + precise). With this, **cell-boxing becomes the sole coherence mechanism**, and env and locals are always
consistent via shared `ContainerRef` cells (or precise write-back) = the goal is realized.

**Verification**: the flip reaches **exactly the same state** as the double-OFF measurement harness
(`MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1`), and that state was already verified by (1) the authoritative
roast survey (whitelist 1285, `run-roast-test.sh`) = 0 failures and (2) the full `make test` (`t/` 10135) = PASS. After the
flip, the default build passes make test 10135 and a diverse roast spot-check (lazy-lists / lazy / gather / throttle /
grep / map / caller / callframe) all PASS.

**Remaining = mechanical cleanup** (behavior-preserving): `env_dirty` (344 uses) / `reconcile_locals_from_env_at_site` (33) /
`ensure_locals_synced` (11) / `saved_env_dirty` (21) / `cell_boxing_active()` (24, always true) are now dead code.
Physically remove them in stages (reconcile is already a no-op, so it is safe). **perf**: every captured-mutated scalar now
becomes a Mutex cell on every access, so check the fib / method-call wall-clock in parallel with the cleanup (§8 hazard
checklist).

### 10.19 Handoff — roast double-OFF 2 remaining (after S18, 2026-06-22)

S4–S18 (16 slices) took the **roast double-OFF surface 25 → 2**. The remaining 2 confirmed by the double-OFF sweep of the
full whitelist (1285) (`MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1` release) (**both are separate axes, not
writeback**; zero new regressions):

| file | Subtests | Kind | Next move |
|---|---|---|---|
| `S02-types/lazy-lists.t` | 14,16 | **true laziness blocker** | `grep is lazy`/`map is lazy`. **Precisely isolated (below)** = blanket reconcile is load-bearing for laziness maintenance. Cannot be solved by precise writeback. Before env_dirty removal, **true lazification so grep/map do not eagerly force a `.lazy` gather** is needed. |
| `S17-supply/throttle.t` | 3,4 | **separate axis (timing)** | A timing assert on a `.5–.8 second gap`. Flaky family. Likely unrelated to env_dirty (if no deterministic pin is possible, drop it from the removal blockers). |

**★Precise isolation of lazy-lists (important; required reading for the next session)**: consume
`gather { take $_ for 0..^$n; $was-lazy = 0 }.lazy` via `grep(*.is-prime)[^3]` and `ok $was-lazy` (if lazy, the gather never
completes, the `$was-lazy=0` line never runs, and it stays 1). **`$was-lazy=0` with blanket-only OFF and with precise-only
OFF (only the both-ON default gives 1)**. Instrumenting a take counter shows the gather is **eagerly forced**
(takes=10) under double-OFF / blanket-OFF, but normal is lazy (grep takes 3 and stops before the take count accrues).
∴ **Precise writeback (snapshot diff) cannot solve it = blanket reconcile is load-bearing for the "do not eagerly force a
`.lazy` gather" laziness maintenance**. Physical env_dirty removal presupposes this true lazification.
Reproduction = `my $w=1; sub mk($n){gather{take $_ for 0..^$n; $w=0}.lazy};
$w=1; my \one=mk(10); one.grep(*.is-prime)[^3]; say $w` (normal=1 / double-OFF=0).

**∴ Writeback candidates are exhausted** (consumed by S16 proto-multi, S17 custom-HOW, S18 EVAL container-slot scalar
writeback). **The remaining 2 are both separate axes, not writeback** (laziness/timing). **★terminator was a parser
misclassification whose substance was EVAL writeback (consumed by S18) = the lesson that the handoff's "separate axis"
classifications need re-verification**. **Next-session order of attack**: (1) **true lazification of lazy-lists** (grep/map
on lazy gather; the confirmed top blocker) → (2) final check whether throttle is flaky → (3) §7.4 / PLAN §2-E (physical
env_dirty removal: hollow out `blanket_reconcile_if_dirty`/`reconcile_locals_from_env_at_site` → remove
`env_dirty`/`ensure_locals_synced`/`saved_env_dirty` → remove the `cell_boxing_active()` gate). Diagnostic file =
`tmp/roast-double-off-final-fails.txt`.
**★Common lesson of S16/S17**: dispatch via the slow path (`run_instance_method_resolved`) (proto-multi candidates, custom
HOW methods) propagates captured writes to env but does not set `env_dirty` = not even the blanket pull fires, and the slot
is stale even in the default build. The fix is an env scalar snapshot diff around the dispatch → record into
`pending_caller_var_writeback` → drain at the calling op. **Ungated is fine** (scope limited to custom dispatch; a subset of
blanket; also fixes default latent bugs). **The slot-only diagnosis method** = compare the same variable via `{ $x }()`
(closure = env read) and directly (slot read); a difference means slot-only = solvable by snapshot diff.
