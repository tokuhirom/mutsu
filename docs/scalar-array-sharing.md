# `$x = @arr` reference sharing — design for first-class containers (bug ②)

> **Status:** Slice 2a + chained `$r = $q` + **Slice 2b (`@aoa[i] = @row` / `%h<k> = @row`
> element / hash-value sharing, 2026-06-19, see §5.2)** IMPLEMENTED. `$scalar = @arr` /
> `$scalar = %hash` (both VarDecl and reassign) now promote the source to a shared `ContainerRef`
> cell, and `.push`/`.unshift`/`.pop`/element writes/whole-reassign propagate bidirectionally.
> pin=`t/scalar-array-share.t`(24). **Side fix**: cell-mediated array push
> (the `ArrayPush` ContainerRef branch in `vm_data_ops.rs`) was made COW via `Arc::make_mut`,
> which also fixed a **pre-existing `:=` bug** where a copy taken from a cell (`my @copy = @z`)
> leaked the source's in-place mutations (`my @a:=@b; my @copy=@a; @a.push` leaked into @copy).
> **Deferred (handled in Slice 2b/2c)**: chained scalar share `my $r = $q`
> (RHS is a scalar var holding an array) = open-q #3; array-element/hash-value assignment `@aoa[i]=@row`
> = Slice 2b.
>
> The rest is the original DESIGN memo (2026-06-18). After finishing for-rw site A of env↔locals
> coherence Stage 1 (array #3259 / hash #3260), the root cause of the remaining **bug ②**
> (`my @a := @$n` non-propagation) was pinned down to "`$x = @arr` *copies* instead of sharing a
> reference"; this document designs the proper fix. It is an extension of
> `docs/container-identity.md` (first-class containers) Phase 1/2 and uses the same ContainerRef
> cell mechanism as the outer-cell promotion in `docs/env-locals-coherence.md` Stage 1.

---

## 0. TL;DR

In Raku, **assigning an Array/Hash to a *scalar slot* shares the same object by reference** (Array is a reference type):

```raku
my @z = (1,2);
my $n = @z;       # $n points at the same Array as @z (not a copy)
@z.push(9);       # → $n is also [1 2 9]
$n.push(8);       # → @z is also [... 8]
```

Assignment to an `@` variable (`my @copy = @z`) is an **element copy** (independent). The distinction is the **destination sigil/slot kind**:
- **`@` variable** ← copy (independent). **Currently correct.**
- **`$` variable / array element (`@aoa[i]`) / hash value (`%h<k>`)** ← share (same object). **Currently a copy = bug.**

`my $n := @z` (bind) shares correctly in mutsu too. The only broken case is **an array entering a scalar slot via `=` (assignment)**.

---

## 1. Precise breakdown of the current state (2026-06-18 probe)

| Operation | mutsu | raku | Verdict |
|---|---|---|---|
| `my @copy = @z; @z.push(9); say @copy` | `[1 2]` | `[1 2]` | ✅ copy correct |
| `my $n = @z; @z.push(9); say $n` | `[1 2]` | `[1 2 9]` | ❌ should share |
| `my $n = @z; $n[0]=8; say @z` | `[8 2]` | `[8 2]` | ✅ **element writes already propagate** |
| `my $n = @z; $n.push(8); say @z` | `[1 2]` | `[1 2 8]` | ❌ **push detaches** |
| `@aoa[0]=@row; @row.push(9); say @aoa[0]` | `[1 2]` | `[1 2 9]` | ❌ should share |
| `%h<k>=@row; @row.push(9); say %h<k>` | `[1 2]` | `[1 2 9]` | ❌ should share |
| `my $n := @z; @z.push(9); say $n` | `[1 2 9]` | `[1 2 9]` | ✅ bind shares |

**Core insight**: `$n = @z` **already shares the same outer `Arc`** (evidence: `$n[0]=8` propagates to @z).
But **`.push`/structural mutation goes through `Arc::make_mut`, which deep-copies when strong_count>1 → detach**, so it does not propagate.
∴ The problem is not "no initial sharing" but "**structural mutation splits via COW**". The same issue that element writes already pass through in-place
(`assign_element_slot`) remains for whole-container structural mutation (push/unshift/reassign).

---

## 2. Why the ContainerRef cell is the canonical solution

`ContainerRef(Arc<Mutex<Value>>)` keeps **the `Arc<Mutex>` shared across clones** even after a COW deep copy
(`docs/env-locals-coherence.md` §2). If we **wrap the array in a cell and have the source (`@z`) and the target (`$n`/element/hash value)
hold the same cell**:
- `.push` (structural mutation) also locks the cell and mutates in place → visible to both (no make_mut detach).
- Element writes are already cell-aware (`assign_element_slot`/`hash_insert_through`).
- Reads are already deconted via `into_deref()` (confirmed in the Stage 0 audit).

This is the **storage-side** counterpart of the same cell mechanism that the for-rw site A fix (#3259/#3260) used via `write_back_container_source`.

---

## 3. Design: escape-aware cell sharing for array assignment into scalar slots

### 3.1 Trigger (when to cell-ify)

At the moment an array/hash value is **stored into a scalar slot**, put the source variable and the target on a shared cell:
1. **Scalar var assignment** `$n = @z` — `SetLocal`/`AssignExpr` where the RHS is Array/Hash and the LHS is `$`.
2. **Array element assignment** `@aoa[i] = @row` — value in `assign_element_slot` is Array/Hash.
3. **Hash value assignment** `%h<k> = @row` — value in `hash_insert_through` is Array/Hash.
4. **(Future)** passing an array to a scalar sub param `f($x)` where `$x` binds an array, return values, etc.

`@` variable assignment (`@copy = @z`) and list assignment are **out of scope** (copying is kept).

### 3.2 Source promotion (escape-aware)

For `$n = @z`, if the RHS Array is a **bare Arc** (not a cell), **promote the source variable `@z` to a ContainerRef cell**
and store that cell into `$n` as well. If `@z` is already a cell, share it. **When the source is an anonymous temporary
(`$n = (1,2,3)` / `$n = @a.map(...)`)**, no promotion is needed = simply wrap it in a cell held only by `$n` (shared with no one,
but if later passed to another scalar, the same cell gets shared).

This generalizes what `box_captured_lexicals` (`vm_register_ops.rs`) does — it currently skips `@`/`%` — into **promotion at the
point of escape into a scalar slot** (the same "cell-on-escape" discipline as closure capture).

### 3.3 Bare locals stay as-is (avoiding the perf cliff — the #2746 lesson)

A bare `@arr` that never escapes into a scalar slot **remains a plain Arc**. This avoids "deref everywhere", where value ops
(arithmetic folds, iteration, native raw-items) would have to decont a ContainerRef on every access. Promotion happens only at escape points.

---

## 4. Blast radius (storage sites) — pre-work audit

Overlaps with the write-site audit of `docs/env-locals-coherence.md` §5 Stage 1. Paths that store array/hash values into scalar slots:
- **Scalar var assignment**: `SetLocal`/`AssignExprLocal`/`AssignExpr`/`SetGlobal` (`vm.rs`/`vm_var_assign_ops.rs`).
  Needs RHS type check + LHS sigil check.
- **Array element assignment**: `assign_element_slot` (`value/mod.rs`) — cell-ify when the value is Array/Hash.
- **Hash value assignment**: `hash_insert_through` (`value/mod.rs`) — same.
- **Read consumers**: `into_deref` (done) + native methods/slices/`.raku`/`.kv` that walk raw-items (leak audit — known Phase 2 issue).

**Hazard**: full coverage of paths where a cell leaks into value ops (open-q#1). The Stage 0 audit unified the read
chokepoints, but an array stored in a scalar flows through **scalar-value-method dispatch** via `$n.method`, so we must
verify it is deconted there (a complement of the same shape as the `deref_container` we added for for-rw site A may be needed).

---

## 5. Incremental slices (no big-bang)

1. **Slice 2a — source promotion for scalar var assignment [DONE 2026-06-18]**: cell-share only `$n = @z`.
   - **Implementation**: new opcode `MarkArrayShareContext` + flag `array_share_context` (set by
     `MarkArrayShareContext`). The compiler's `try_emit_array_share` (`compiler/stmt.rs`) detects a scalar LHS +
     `ArrayVar`/`HashVar` RHS, wraps the RHS value in `WrapVarRef(source)`, and emits `MarkArrayShareContext`.
     The VM, in `array_share_assign` (`vm_var_assign_ops.rs`), promotes source/target to a shared `ContainerRef` cell
     and sets a `__mutsu_array_share::<name>` marker on the scalar slot. Promotion and replace-on-reassign were added
     to **all 3 paths — SetLocal(VarDecl) / AssignExpr(reassign) / SetLocal slow path**
     (once `$n` holds an array, `simple_locals=false` and it goes through the slow path, so the fast path alone was
     insufficient — the biggest pitfall during implementation). `our`/globals consume the flag in SetGlobal (copy; Slice 2d).
   - **rebind vs mutate-through (implemented as designed)**: a whole-reassign of the scalar to a non-array/different
     container drops the `__mutsu_array_share` marker and replaces the slot, while `@z = (...)` (array var) and `.push`
     write through the cell. The marker distinguishes `:=` write-through from `=` replace on a per-variable basis.
   (Old design memo:) cell-share only `$n = @z`.
   Pin `@z.push`/`$n.push` bidirectional propagation (`t/scalar-array-share.t`). Guard that `@copy = @z` is unchanged (copy kept).
   Acceptance = reads and existing roast match exactly. **Minimal slice.**
   - **[Implementation insight 2026-06-18] The RHS source is identifiable at compile time**: `my $n = @z` compiles to
     `VarDecl { name:"n", expr: ArrayVar("z") }` (confirmed with `--dump-ast`). ∴ The source variable name "z" is known at
     compile time = without building new varref tracking for `:=`, we can detect **scalar LHS + `ArrayVar`/`HashVar` RHS**
     and emit a source-promotion op. Reassignment `$n = @z` (non-decl) can likewise be checked in `AssignExpr` for an `ArrayVar` RHS.
   - **★The rebind vs mutate-through distinction is the crux**: after cell sharing,
     - `$n.push` / `@z.push` / `$n[0]=x` / `@z[0]=x` → lock the cell and mutate in place (bidirectional propagation).
     - `$n = 5` (reassign the scalar to a non-array) → **replace $n's slot with `Value::Int(5)`**
       (do not touch the cell = @z unchanged).
       `$n = @other` (different array) → repoint $n's slot to @other's cell (@z's cell unchanged).
     - `@z = (9)` (array var reassign) → **mutate @z's cell through** (contents replaced = visible to $n).
     In other words: "non-array assignment / different-container assignment into a scalar slot = slot replacement",
     "array var assignment = cell content replacement".
     Unlike a `:=` bind (sharing the scalar container itself), **only the array object (the cell) is shared; the scalar
     container is separate**. SetLocal's current behavior of "keep the existing ContainerRef and write the contents through"
     (the `vm_var_assign_ops.rs:5543` family) must branch into "slot replacement" for non-array assignment into a scalar.
2. **Slice 2b — array element / hash value assignment [DONE 2026-06-19 / PR #3274]**: cell-share `@aoa[i] = @row` /
   `%h<k> = @row`. Reference sharing for AoA and HoA.
   - **Implementation (2026-06-19)**: rather than writing from scratch, we **reused the `:=` element-bind mechanism**.
     The compiler (`element_share_bind_value` in `compiler/expr_closure.rs`) detects `@var[single subscript] = @containervar`,
     wraps the RHS in `__mutsu_bind_index_value`, and compiles it to **the same bytecode as a bind** (the bind mechanism does
     the cell installation + source promotion as-is) + emits the new opcode `MarkElementShare`. The VM consumes
     `element_share_pending` in `exec_index_assign_expr_named_op` and, after the store, sets an **element-keyed marker**
     `__mutsu_elem_share::<var>` (a Hash of encoded-index; `mark_element_share`/`is_element_share`/`clear_element_share` —
     the per-variable marker of scalar 2a extended to per-element). **The single difference between `=` and `:=` = replace vs
     write-through on reassignment** is branched into replace via the `elem_is_value_share` gate at the 3 write-through
     chokepoints (array `arr[i]` cell branch = the old BLOCKER around line ~3836 / hash slow `hash_insert_through` / hash fast
     `try_fast_hash_element_assign` = bails on ContainerRef and sends to slow) (precompute happens before the container borrow,
     marker clear after the store).
     ∴ The "per-element share-vs-bind distinction" that §10/the old BLOCKER pointed out is realized via the element-keyed marker.
   - **★Pitfalls**: ① **false positives on slice assignment** — `@b[*-3,*-2] = @x` is a distributing slice assignment, not
     reference sharing. Do not share when the index is `ArrayLiteral`/Range/Whatever (whitelist only single scalar subscripts =
     `Literal(Int/Str/Num/Bool)`/`Var`/`Binary`/`Unary`). ② **self-reference** `%h<k> = %h` (infinite HoH) must keep the existing
     `is_self_hash_ref`→`self_hash_ref_marker` path; promoting the source (=target) to ContainerRef breaks
     `isa-ok %h, Hash` and circular reads (hash_ref.t whitelist regression) → exclude from sharing when `source == target_name`.
   - pin=`t/element-array-share.t` (28 cases). make test PASS, whitelist S02/S03 all 251 PASS.
   - **[chained `$r = $q` DONE 2026-06-18, open-q#3 / #3267]**: chained sharing with a scalar source is also implemented.
     Slice 2a's `WrapVarRef`+`MarkArrayShareContext` was refactored into a unified
     **`MarkArrayShareSource(name_idx)` + `array_share_source` field** (WrapVarRef had the side effect of turning plain
     `$x=$y` into a bind, so it could not be used). The compiler's `try_emit_array_share` also accepts an `Expr::Var(n)` RHS.
     At runtime, sharing happens **only when the source value derefs to Array/Hash** (`with_deref` gate) → plain `$x=$y`
     (scalar source) and chained `=` of a `:=`-bound Int scalar keep copying. 9 cases added to the pin (33 total).
     make test/roast green.
3. **Slice 2c — the deref bind of bug ② [array form DONE 2026-06-18 (6th) / PR #3268]**: made `my @a := @$n` (deref bind of a
   value-alias scalar `$n`) follow 2a's shared cell and reach the caller's @z.
   - **Implementation**: the parser desugars `@$n` to `ArrayVar("n")`, so the bind source is `"@n"` (WrapVarRef).
     The actual cell lives on the scalar `$n` (env key `"n"`). Added a **scalar-source fallback** to the SetLocal bind path
     (the `if let Some(source_name) = bind_source` block in `vm_var_assign_ops.rs`):
     when `resolved_source` ("@n") is **not a container value at runtime** (env.get("@n") is not Array/Hash/ContainerRef),
     and the scalar under the bare name with `@`/`%` stripped ("n") holds a `ContainerRef`, reuse that cell.
     `effective_source` points promotion/env/frame write-back at the bare scalar name. pin=`t/deref-bind-value-alias.t`.
   - **★Pitfall**: `source_in_same_scope` (whether "@n" is in `code.locals`) looks at the **locals of the whole function**,
     so a `my @n` in a *different block* also makes it true and wrongly suppressed the fallback. ∴ The gate must be
     **whether resolved_source's runtime value is a container** (`source_resolves_to_container`), not locals membership.
   - **hash form `my %h := %$m` [DONE 2026-06-19 (24th) / PR pending]**: the parser desugars `%$m` to `$m.hash`
     (MethodCall), so it did not ride the simple-var bind path like `ArrayVar`. **Fix** =
     in the bind context (`handle_binding`, `my_decl_assign.rs`), when the target is a hash and the RHS is a plain-scalar-shaped
     `$m.hash` (`MethodCall{target:Var(m), name:"hash", args:[]}`), rewrite to `HashVar("m")`.
     This puts it on the same deref-bind path as the array form (the scalar-source fallback already handles both `['@','%']`).
     CaptureVar forms like `%$/` and `%$_` stay as `.hash` coercion (they are not value-aliases).
   - **★Symmetric bug discovered and fixed at the same time (also present for arrays)**: **element assignment** through a deref
     bind (`@a[0]=` / `%h<k>=`) did not write through the shared cell and detached (`.push` worked via a different opcode).
     Root cause = the SetLocal bind path sets `__mutsu_sigilless_alias::%h = "%m"` (sigiled source name), but index-assign
     follows this alias to a nonexistent `%m` and autovivifies it → fresh detached container. **Fix** = when `scalar_source`
     is Some (deref bind via a promoted scalar), repoint the alias to **effective_source** (the bare scalar "m").
     Index-assign then reaches the shared cell. Array `@a[0]=` was fixed at the same time.
   - **Remaining: via sub params (the headline bug② `sub f($n){ my @a := @$n; @a.push }`)**: the param `$n` propagates on
     direct `.push` (case A), but stacking a deref bind on top does not propagate = the param does not hold a
     `ContainerRef` under the `"n"` env key (`arg_sources` return-writeback sharing = a different mechanism). Slice 2d's
     param-by-reference work is the prerequisite.
4. **Slice 2d — array/hash variable → scalar param sharing [✅ DONE 2026-06-19, PR #3283]**: resolves the headline bug②
   (`sub f($n){ my @a := @$n; @a.push }; f(@z)` gives mutsu [1 2] / raku [1 2 99]). **Session 24 judged this the
   "Slice F wall", but session 25's precise probe overturned that**:
   - **Re-isolation of the root cause**: sharing across the call boundary itself is not broken. `$n.push`/`$n[0]=`
     (direct mutation) already propagated to the caller's @z — but via **env_dirty-driven copy-back, fragile and
     statement-order-dependent** (inserting a single `say` statement before `$n.push` breaks it). **The local case
     (`my $n=@z; my @a:=@$n; @a.push`) works fully with Slice 2a/2c.** ∴ The only thing broken is that the param `$n`
     does not hold a shared cell.
   - **Solution (adopted) = at the call boundary, when passing an `@`/`%` variable to a `$`-param, promote the value
     to a shared `ContainerRef` cell** (`bind_function_args_values`/binding.rs) + register rw_bindings (the existing
     `apply_rw_bindings_to_env` writeback flushes to the caller on return). `my @a:=@$n` rides Slice 2c's deref-bind
     cell sharing, and `$n.push` is also robust via the cell.
   - **★Biggest pitfall = the slot-only fast paths (`positional_light`/`light`) bypass `bind_function_args_values`**
     and bind params directly into local slots (this was the real identity of session 24's "wall"). **Solution = gate
     `call_shares_container_into_scalar_param` (`vm_call_dispatch.rs`) detects "calls passing an `@`/`%` array/hash
     variable to a plain `$`-param" and diverts all fast paths to the slow path**. Wired at 5 dispatch sites (pos_light
     cache / light cache = `exec_call_func_op` + otf block + the 2 fast paths of `dispatch_func_call_inner`). Args-first
     evaluation rejects non-container args (fib) immediately = perf preserved.
   - **Two representation traps**: ① a plain `$`-param is stored with the sigil stripped, `"n"` (@/% keep theirs) =
     `is_plain_scalar_param_name`. ② variable arguments arrive as `Value::Capture { __mutsu_varref_value/name }` =
     peek via `arg_is_container_value`.
   - **★First CI red → fix-forward**: the first version promoted "whenever the value is an Array", which **also fired
     when a `$`-scalar holding an array is passed ($aref→$refin)** → writeback turned the caller's `$aref` into a
     ContainerRef and broke `$aref[0]++` (roast S06-signature/named-parameters 68-69). **Fix = promote only for
     `@`/`%` sources** (scalar sources already work for element mutation via the existing Arc sharing = promotion is
     unnecessary and harmful). pin=`t/scalar-param-container-share.t`(21).
   - **Remaining follow-ups (not covered by 2d, pre-existing, not regressions)**:
     - **method params** (`method m($n){ $n.push }`) = raku shares / mutsu does not. **Extension point = add a
       scalar-container-share condition to the fast-path gate at `vm_method_dispatch.rs:79`
       (`!has_rw_params && !has_aliasable_container_params`)** (the slow path uses `bind_function_args_values` at
       `vm_method_dispatch.rs:424` → binding.rs promotion applies). Precedent = `has_aliasable_container_params` in the
       same file (already excludes fast path in the same shape for @/% params). **Caution = method param_defs include
       the invocant → index alignment with args differs from subs** (dedicated logic needed to align non-invocant
       positionals with args). `call_shares_container_into_scalar_param` can be refactored to be param_defs-based and reused.
     - **`is copy` $-params** = raku shares the array (copy copies the container binding; the array is the same) /
       mutsu does not share. The gate currently excludes them via `traits.is_empty()`. Take care to reconcile allowing
       `$n=…` rebind with cell sharing.
   - **Lesson**: CI roast logs are truncated, making it hard to identify failing files → run
     `prove -j4 $(cat roast-whitelist.txt)` locally in full and look for concrete subtests with Failed:[1-9]
     (Failed:0/exited255 are load flakes; encoding-related ones fail on main too = red herrings).

Each Slice is hardened with **make test (t/ regressions are not in the whitelist = mandatory) + a release-roast
main-vs-branch comparison + int.t / method-call wall-clock** (the #2746 lesson: perf regressions only surface as roast timeouts).

---

## 6. open questions

1. **Decont in scalar-value-method dispatch**: do `$n.push`/`$n.elems` etc. decont the cell before reaching the native
   array method? A leak here is the entrance to "deref everywhere".
2. **Reliable separation from `@copy = @z`**: can the destination sigil/slot check misfire for list-assign / flatten /
   slice assignment?
3. **Handling anonymous temporaries**: `$n = (1,2,3)` shares with no one, but do we wrap it in a cell? (If wrapped, a
   subsequent `$m = $n` share falls out naturally.) If not wrapped, `$m = $n` sharing needs separate work later.
4. **Cross-thread**: consistency of the shared cell (`Arc<Mutex>`) with `clone_for_thread`/`shared_vars` (Track C).
5. **COW cost**: the cost of value ops locking a cell-ified array on every access. Escape-awareness excludes bare
   locals, but measure wall-clock for the hot path of an array in a scalar (`$n[i]` loops).

---

## 8. Result of the implementation attempt (2026-06-18) — Slice 2a works but was reverted due to Arc-identity ripple

> **★SUPERSEDED (2026-06-18, PR #3264 MERGED)**: this §8 is the record of a parallel attempt using
> `MarkValueAliasSource` that was reverted due to a Pair-value regression. **Slice 2a subsequently landed
> for real in PR #3264** (with a different opcode, `MarkArrayShareContext`). The Arc-identity ripple that §8
> declared "impossible" was resolved **without first landing a blanket Sub-slice 1a (behavior-invariant decont of
> all write chokepoints)**, by **individually making the identity mechanisms that the cell broke cell-aware**:
> ① `overwrite_array/hash_bindings_by_identity` (write through the contents of ContainerRef cells whose inner
> holds the needle Arc) ② deref `current` in `builtin_index_assign_method_lvalue` ③ regex
> `interpolate_regex_scalars` var reads via `into_deref` ④ make `UndefineAggregate` cell-aware
> (`clear_aggregate_cell`). **∴ The incremental "plug each leak with a deref as you find it" approach is
> sufficient**; a blanket Stage 0 1a upfront was unnecessary. The DONE note in §5 Slice 2a is the current
> implementation truth. The §8 body below is a historical record.

Slice 2a (cell sharing for scalar `$n = @z` = "value-alias") was implemented and verified. **The design is correct
and confirmed the slice ordering of §5.** Conclusion: **value-alias breaks the existing Arc-identity mechanisms
unless Sub-slice 1a (write-chokepoint decont) lands first.** Reverted.

### Implementation that worked (reusable as-is next time)

- New opcode `MarkValueAliasSource(name_idx)` (`opcode.rs`), emitted by the compiler right before SetLocal for
  `my $n = @z`/`$n = @z` (scalar = whole `@`/`%` var) (`compiler/stmt.rs` VarDecl else branch + the
  Stmt::Assign plain-assign branch, **local scalars only** = SetLocal consumes the flag; with SetGlobal the
  flag would dangle). **gotcha**: the `s` in `Expr::ArrayVar(s)` is **sigil-less** ("z"). The src must add the
  sigil via `format!("@{}", s)` or it will not match the local slot "@z" (this made the share silently no-op at first).
- VM: `value_alias_source: Option<String>` field (`runtime/mod.rs`). At the top of SetLocal, take it and call
  `setup_scalar_value_alias`: cell-ify src as a shared `ContainerRef` (same shape as the bind path at
  `vm_var_assign_ops.rs:6192` — write back to slot+env+saved frames), give $n the same cell, and put a
  `__mutsu_value_alias::$n` marker in the env. The snapshot (the deref value from GetArrayVar) is the initial
  value when no cell exists.
- detach (§5 Slice 2a's "non-array assignment into a scalar = slot replacement"): reassignment of a ContainerRef
  scalar carrying the value-alias marker **replaces the slot without writing through the cell** (`$n = 5` does not
  break @z). **gotcha**: a value-alias scalar becomes `simple_locals=false` so reads go through the env; detach
  must overwrite the env directly with `env_mut().insert(name, v)`, not `flush_local_to_env` (which is a no-op
  outside simple locals) — otherwise a stale ContainerRef remains and `say $n` reads the old array.

### Verification results

- **All core cases PASS** (`t/scalar-array-ref-sharing.t`, 18 cases): `my $n=@z; $n.push(99)` → @z has 3 elements,
  element/push bidirectional propagation, detach (including after push), the hash variant, itemized copy
  (`my @c=$n` → `[[1 2] 3]`, matches raku).
- `cargo test` 466/0, array/binding/list/signature roast spread clean.

### Reason for revert — open-q#1/#2 are real walls

Making `$a = @src` turn $a into a **ContainerRef breaks the existing mechanisms that track the source by Arc identity**:
**`t/pair-value-element-writethrough.t` (#2943) regressed** — the write-through of
`$p = (k => $a); $p.value[3] = "x"` matches @src/$a's **plain Array Arc by identity**, but once cell-ified into
`Value::ContainerRef` the match fails and the write goes nowhere. This is exactly the dependency §5 described as
"Stage 0 chokepoints must precede 2a" = **Sub-slice 1a, making all mutation/write-through sites
ContainerRef-aware (decont), is a prerequisite**. open-q#1 (decont in scalar-value-method dispatch) has the same
root: an audit ensuring the cell never leaks is required.

**∴ The correct order for next time: (1) land Sub-slice 1a behavior-invariant (decont ContainerRef at all
write chokepoints; roast byte-identical = acceptance) → (2) re-apply value-alias (this implementation) on top.**
A standalone value-alias PR breaks the Arc-identity mechanisms and is not viable.

---

## 9. References

- `docs/env-locals-coherence.md` — Stage 1 outer-cell promotion (for-rw site A done). This document is its
  storage-side extension.
- `docs/container-identity.md` — first-class container ledger (Phase 1 scalar cells / Phase 2 element cells).
- `docs/vm-single-store.md` — dual-store unification (Slice F). Scalar-array sharing also presupposes env↔locals
  sharing the same cell.
- Empirical probe: the table in §1 (2026-06-18). Implementation attempt: §8 (2026-06-18).

---

## 10. Slice 2b kickoff handoff (recon completed 2026-06-18 — for immediate start next session)

Make `@aoa[i] = @row` / `%h<k> = @row` (whole `@`/`%` assignment into an array element / hash value) share by
reference. **Slice 2a's cell mechanism + `MarkArrayShareSource`/`array_share_source` can be reused as-is.** The
implementation map, raku semantics, leak assessment, and the biggest design issue found during recon are pinned below.

### 10.1 Current state (2026-06-18 probe — all are mutsu=copy / raku=share bugs)

| Operation | mutsu | raku |
|---|---|---|
| `@aoa[0]=@row; @row.push(9); say @aoa[0]` | `[1 2]` | `[1 2 9]` |
| `@aoa[0]=@row; @aoa[0].push(8); say @row` | no propagation | `[1 2 ... 8]` |
| `%h<k>=@v; @v.push(3); say %h<k>` | `[1 2]` | `[1 2 3]` |

### 10.2 ★Most important: the cell-install mechanism **already exists** in its `:=` form = just widen the trigger to `=`

`@aoa[0] := @row` / `%h<k> := @v` (`:=` element/value bind) **already shares fully bidirectionally** in mutsu
(probe-confirmed: `@aoa[0].push`→@row, `@v.push`→%h<k>, both directions OK). In other words, **the storage and
read mechanisms for element cells are complete**. Slice 2b is "do for `=` (via array_share_source) what `:=` already does".
- **named handler** (`@aoa[0]=@row`: target is `ArrayVar`/`HashVar`) = `IndexAssignExprNamed` →
  `exec_index_assign_expr_named_op_inner` (`vm_var_assign_ops.rs:2772`). **The `bind_cell` mechanism gated by
  `bind_mode` is already there** (install the cell into the element + write the cell back to the source var). Fire
  it for `array_share_source` too.
- **generic/computed handler** (`($ref)[i]=@row`, `f()<k>=@row`) = the same-shaped `bind_cell` mechanism at
  `vm_var_assign_ops.rs:4787-4897`. Handle `=` likewise.
- **compile hook**: `compile_expr_index_assign` (`compiler/expr_closure.rs:286`). `@aoa[0]=@row` is
  `IndexAssign{target:ArrayVar("aoa"), index, value:ArrayVar("row")}` (confirmed with `--dump-ast`). Right after
  compiling the value with `compile_bind_index_value` (:268), **emit `MarkArrayShareSource(value_source_name)` when
  the value is `ArrayVar`/`HashVar`** (same opcode/field as 2a). Only when the target is also
  `ArrayVar`/`HashVar` (element LHS).

### 10.3 raku semantics (probe-confirmed — elements have the same rebind/replace as 2a's `$n`)

- `@aoa[0]=@row` → share (cell). `@row.push`/`@aoa[0].push` bidirectional.
- `@aoa[0]=5` (reassign the element to a non-array) → **replace the element slot with 5; @row unchanged** (`[1 2]`).
- `@aoa[0]=@other` (different array) → repoint to @other's cell; @row is no longer followed; only `@other.push` propagates.
- Hash values likewise (`%h<k>=@v` shares / `%h<k>=5` replaces / `%h<k>=@other` repoints).

### 10.4 ★Biggest design issue: element cells have no name marker (distinguishing `=`-share from `:=`-bind)

2a used the `__mutsu_array_share::<name>` marker to implement "whole-reassign of an `=`-share replaces the slot
instead of writing through the cell", but **elements have no variable name, so the same marker cannot be attached**.
**★Concrete BLOCKER site (identified by the user in #3268)**: the single-element array store
`exec_index_assign_expr_named_op_inner` (`vm_var_assign_ops.rs` ~line 3836) has
`else if let Value::ContainerRef(cell) = &arr[i] { *cell.lock()=val }`, which **unconditionally writes through a
ContainerRef element on reassignment**. An `=`-share wants replace, but a bind-cell wants write-through.
`assign_element_slot` (`value/mod.rs:3009`) has the same shape.
- **★Probe-confirmed (2026-06-18)**: in raku, after `@aoa[0]:=@row`, **both** `@aoa[0]=5` **and** `@aoa[0]=(7,7)`
  are "Cannot assign to an immutable value" **errors** (neither non-array nor another array write through).
  ∴ **No correct raku path "writes a value through an element cell"** (mutsu's current write-through was
  non-conformant to begin with).
- **Direction = option A (branch on new value type)**: on element write, new value is an array (and
  array_share_source is set) → re-share (repoint to a different cell) / new value is a non-array → replace the
  element slot (discard the cell). No name needed. **Prerequisite, however**: some cases in the existing
  `t/element-bind-cell.t` (`:=`-bound elements) depend on the ~3836 write-through (the user's point: "a
  per-element share-vs-bind distinction is needed") → **when starting, enumerate the write-throughs that
  element-bind-cell.t requires, and branch only `=`-share into replace** (bound-index marker present =
  keep write-through / cell originating from array_share_source = replace). The shortcut of "push a
  ContainerRef on the value side and let the normal store handle it" collapses on this write-through — not viable.
- **Option B (alternative)**: give the cell a "value-share" flag (new field/wrapper) for per-cell distinction.
  Fallback if option A cannot coexist with element-bind-cell.t.

### 10.5 Leak assessment: **2b is lower-risk than 2a**

Warm-up probe of the element-cell read surface via `@aoa[0]:=@row` (deref read/`.elems`/parent stringify/
nested index `@aoa[0][1]`/`for @aoa`/`@aoa.map`/itemize/`.WHAT`/`.raku`) → **all match raku**
(`element-bind-cell.t` already covers these). The only trivial difference = the single-element trailing comma in
`@aoa.raku` (`[[1,2,9]]` vs `[[1,2,9],]` — pre-existing, unrelated to cells). ∴ The element-cell read leak surface
is well-worn. The regex/undefine-class leaks 2a stepped on are unlikely for elements (elements are concentrated in
into_deref/resolve_*_entry). Still, when starting, re-run the same warm-up probe with `=`-share to confirm.

### 10.6 Pin-test draft (append to `t/scalar-array-share.t` or new `t/scalar-array-share-2b.t`)

```raku
# AoA element share
my @row = (1,2); my @aoa; @aoa[0] = @row;
@row.push(9);      is-deeply @aoa[0].Array, [1,2,9];     # @row.push -> element
@aoa[0].push(8);   is-deeply @row.Array, [1,2,9,8];      # element.push -> @row
# element reassign to non-array replaces (raku value semantics)
my @r=(1,2); my @a; @a[0]=@r; @a[0]=5; is-deeply @r.Array,[1,2]; is @a[0],5;
# element reassign to another array re-points
my @r2=(1,2); my @o=(7,8); my @b; @b[0]=@r2; @b[0]=@o; @o.push(99);
is-deeply @b[0].Array,[7,8,99]; @r2.push(88); is-deeply @b[0].Array,[7,8,99];
# HoA value share
my @v=(1,2); my %h; %h<k>=@v; @v.push(3); is-deeply %h<k>.Array,[1,2,3];
%h<k>.push(4); is-deeply @v.Array,[1,2,3,4];
# plain element copy unaffected: @x[0]=@y where used as copy? (raku still shares — verify)
```

### 10.7 Verification order

(1) Implement the compile hook + named-handler `=`-share install and pin AoA/HoA bidirectional sharing →
(2) element rebind/replace (§10.4 option A) → (3) `make test` (t/ mandatory) + the single-element
re-share/replace boundary of `MarkArrayShareSource` → (4) generic/computed handler support → (5) `make roast`
for leak/regression checks.
**As with Slice 2a/chained, first sweep the read paths where source promotion turned element values into
ContainerRef with the warm-up probe** (§10.5 confirmed the surface is well-worn, but re-confirm after implementing `=`-share).
