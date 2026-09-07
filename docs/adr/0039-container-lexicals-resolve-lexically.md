# ADR-0039: `@`/`%` lexicals must resolve lexically — retiring by-name container resolution, and correcting the "container write-back is structurally different" premise

- Status: Slice 1 (§4.1) landed 2026-08-20. §8.2's recorded cross-thread
  collisions closed 2026-08-22 by giving a container's bare-name lane entry a
  *lifetime* — see §8.6, which also records two cheaper routes measured and
  rejected. §4.1's excluded `our` container case (§1.2's third instance) was
  fixed separately on 2026-08-23 by a resolution-only change, as §4.1
  predicted it could be — see
  `news/2026-08/our-container-bare-name-prefers-package-mirror.md` and
  `src/vm/vm_our_package_vars.rs`. Its SCALAR twin — which needed a write gate
  as well as a resolution preference, because a scalar write replaces a value —
  followed on 2026-08-25 through the same module's `our` cell; see
  `news/2026-08/our-scalar-bare-name-resolves-to-the-package-cell.md`.
  Slice 2 (§4.2) is still next and still the end state. Its read side was
  re-implemented and re-measured on 2026-09-06 and **withdrawn a second time**;
  §10 records that re-measurement (which found §4.1's exclusion list already
  free of divergences), the four defects the flip exposed, the one fix that
  shipped on its own, and the reason it was blocked then — the *write/capture*
  lane, not §9's store lane. §11 measured that blocker closed. **Attempt 3
  (2026-09-07) got much further and was withdrawn a third time**: it reached a
  green `prove t/` AND a green full `make roast` with the flip on, and was
  stopped by the bundled-library battery gate. §12 records what it measured, the
  seven repairs it needed, and the one blocker left; the work item carries the
  re-derivable detail.
- Date: 2026-08-20
- Related: ADR-0013 (container interior mutability — `gc_contents_mut`),
  ADR-0024 (mainline lexicals for named subs — the scalar half of this bug),
  ADR-0025 (cell boxing must be value-kind-blind — slice 3 defers `@`/`%`),
  ADR-0010 (cross-thread lexical sharing scope — the `__mutsu_atomic_*` lanes)
- Addresses: `news/2026-09/nested-frame-container-mutation-reaches-its-owner.md`
  (originally `todo/deep/module-file-scope-array-and-hash-still-share-the-caller.md`;
  closed 2026-09-06, see §11) and its successor work item
  `todo/deep/adr0039-slice2-container-reads-compile-to-a-slot.md`,
  `news/2026-08/shared-store-bare-name-collision-across-unrelated-frames.md`
  (the cross-thread-store axis of the same root cause — see §8, added 2026-08-20;
  closed 2026-08-22, see §8.6)

## 1. Context

### 1.1 The symptom, as previously recorded

The deep ticket records a module's file-scope `my @a` sharing storage with the
loading script's same-named `my @a`:

```raku
# tmp/ufl/lib/UFL.rakumod
unit module UFL;
my @items = <a b>;
sub peek-items() is export { @items.join(",") }
sub push-item($v) is export { @items.push($v) }
```
```raku
use UFL;
my @items = <x y z>;
push-item("c");
say peek-items();        # raku: a,b,c    mutsu: x,y,z,c
say @items.join(",");    # raku: x,y,z    mutsu: x,y,z,c
```

Re-verified on `bd34751d3` (2026-08-20): the repro still fails exactly as
recorded, and `%h` fails identically. A full operation matrix
(`tmp/ufl/matrix.raku`, 15 assertions over read / `push` / element-assign /
whole-assign / key-set / `:delete`, `@` and `%`) diverges from `raku` on
**every** line except one coincidence — including line 1, the module's own
*initial* value, which the consumer's later `my @arr = <x y>` has already
destroyed by the time the first module routine runs.

### 1.2 What the investigation actually found: this is not a module bug

The ticket frames the collision as a property of module loading — a module body
running in the caller's `env` (`run_modules.rs:812`). That framing is too
narrow. The identical divergence reproduces with **no module involved at all**,
at plain mainline scope (`tmp/ufl/namedsub-mainline.raku`):

```raku
my @names = <a b>;
sub add-name($v) { @names.push($v) }
sub read-names()  { @names.join(",") }
add-name("c");                       # 1: a,b,c        (mutsu agrees)
{
    my @names = <x y z>;             # an ordinary shadowing block
    add-name("d");
    say @names.join(",");            # raku: x,y,z     mutsu: x,y,z,d
    say read-names();                # raku: a,b,c,d   mutsu: x,y,z,d
}
say read-names();                    # raku: a,b,c,d   mutsu: a,b,c
```

The byte-identical **scalar** program (`tmp/ufl/namedsub-scalar.raku`) matches
`raku` on all four lines, because ADR-0024 fixed it: a mainline named sub's
scalar free variable resolves through its own captured
`unit_lexicals[MAINLINE_UNIT_KEY]` cell rather than through whatever the `env`
key currently holds.

A third instance falls out of the same root cause: `our @arr` in a `unit
module` collides too (`tmp/ufl/ourtest.raku`). mutsu *does* maintain the
package-qualified mirror — `@UFL3::arr` still reads the module's own `a,b` —
but the module's own routines never consult it, so `a-push` lands on the
consumer's array.

**Root cause, stated once:** a named sub's `@`/`%` free variable is compiled to
a bare by-name `GetArrayVar("@items")` / `GetHashVar("%h")`
(`compiler/expr.rs:132-141` and its `HashVar` twin) and resolved at run time
against whatever `env` currently holds under that key. Container lexical
scoping is therefore **dynamic**, not lexical. Every same-named declaration
anywhere in the process — a consumer's `my`, an inner block's shadow, a
sibling routine's local — hijacks it. Module loading is merely the shape that
makes the collision most likely (two independently-authored files, one `env`).

This is precisely the follow-up both ADR-0024 and ADR-0025 named and deferred:

- ADR-0024 "Known limitations": *"`@`/`%`/`&` free variables: excluded from
  slice 1 ... Cell-ifying them intersects the ADR-0010 atomic lanes and Track B
  and should be its own slice."* Enforced at `vm/vm_register_sub_ops.rs:464-470`.
- ADR-0025 slice 3: *"`@`/`%`/`&`: reference-shared already; rebinding staleness
  is a narrower hole. Deferred with ADR-0024's identical limitation."*
- `run_modules.rs:977-982` ("**Scalars only.**") enforces the same skip in
  `collect_unit_lexical_names`, the filter itself at `:1002-1008`.

**This ADR owns that deferred follow-up.** The deep ticket is one of its
symptoms, not a separate problem.

### 1.3 Why `@`/`%` are the odd ones out (the compile-time asymmetry)

`@`/`%` locals *are* slot-allocated — `declare_local` pushes the sigiled name
`"@a"` into `code.locals` and `local_map` (`compiler/mod.rs:1688-1707`,
`compiler/stmt.rs:1607`). But **nothing ever reads the slot**:

- `Expr::ArrayVar` / `Expr::HashVar` consult `local_map` *only* to decide
  whether to package-qualify the name, then unconditionally emit
  `GetArrayVar(name)` / `GetHashVar(name)` (`compiler/expr.rs:132-141`,
  `:142-177`). The scalar path emits `GetLocal(slot)`
  (`compiler/expr_helpers.rs:697`).
- `compute_upvalues` excludes them outright:
  `is_plain_user_lexical(s) && !s.starts_with(['@','%','&'])`
  (`opcode.rs:6124-6130`), and its read-op table matches only `GetGlobal`
  (`:6074-6078`). So a container free variable gets no upvalue indirection.
- `is_plain_lexical_name` excludes `@%&` (`compiler/mod.rs:1712-1721`), so
  `plain_locals[slot] == false` and every `SetLocal` on a container takes the
  full env-mirror path (`vm/vm_var_assign_set_local.rs:1863-1878`) — the value
  is unconditionally written to `env["@a"]`.
- A sub body compiles with a fresh, empty `local_map`
  (`compiler/helpers_sub_body.rs:220`) under a mangled `"::&"` package that
  disables `qualify_variable_name` (`compiler/mod.rs:1642-1646`), so `@items`
  emits as the bare `"@items"`. The OTF path
  (`vm/vm_call_dispatch.rs:188-198`) is more isolated still — no
  `inherit_enclosing_scopes` at all.

So the container lane has slots but uses names, while the scalar lane has both
and prefers slots. Every mechanism built on top of the scalar lane
(ADR-0024's cells, upvalues, `authoritative_free_vars`) simply has no container
counterpart to hook into.

## 2. The premise this ADR corrects

The deep ticket's central technical claim — the reason it sized the fix at
"~120+ call sites across at least a dozen files" and deferred it twice — is:

> A container "write" is usually an **in-place mutation through a `Gc` pointer**
> obtained from a *prior* read — `push`/`pop`/`splice`/element-assign call
> `Gc::make_mut` on the array's `Gc<ArrayData>`, which silently **reallocates**
> (breaks aliasing with any other holder) whenever the strong count is above 1
> ... so unlike scalars, a sound fix needs a **write-through-the-canonical-slot**
> primitive for containers, not just a value get/set pair.

**That is no longer true, and it is the load-bearing premise.** Container
mutation in mutsu is not copy-on-write; it is write-through-the-shared-node, by
explicit design. `Value::with_array_inplace` (`value/view.rs:769-789`) says so
in its own doc comment:

> Container-identity in-place mutation (§3): run `f` on the SHARED `ArrayData`
> backing this `Array` value, writing through the node so every by-value holder
> of the same container ... observes the write. **`Gc::make_mut` (COW) is wrong
> for a mutation of a variable's own container** — it detaches the container
> from its aliases the moment the backing is shared; Raku `=` copy semantics are
> enforced at copy time instead (`detach_shared_container`).

It reaches the payload through `gc_contents_mut` — the ADR-0013 §7
interior-mutability primitive. `push_to_shared_var`'s own env fallback
(`runtime/runtime_thread.rs:929-948`) uses the sibling `gc_data_mut` under the
comment *"write through the shared node so same-thread by-value holders observe
the push (container identity §3)."*

Empirically (`tmp/ufl/alias.raku`, mutsu matches `raku` on all four lines): two
**distinct env keys** holding the same container observe each other's `push`,
element-assign, and hash key-set. No reallocation, no lost write, no write-back
step.

The consequence is large. **In-place container mutation needs no canonical-slot
write handle at all** — it only needs to *read* the right container. The
ticket's ~140-site inventory is an inventory of `env` accesses, not of sites
that would need a new write primitive; the great majority are receiver
*resolution* (a read) that a resolver already exists for, or `__mutsu_*`
metadata keys that are not user variables.

Ironically, the ticket's own 2026-08-14 "correction" section declared ADR-0013
orthogonal to this problem ("that is about whether taking `&mut` through a
shared `Gc<T>` pointer is *sound* ... orthogonal to this ticket's problem, which
is **routing**"). The routing diagnosis was right; the dismissal of ADR-0013 was
not — ADR-0013 is exactly why the routing diagnosis is now the *whole* problem
rather than half of it.

## 3. What is already in place

- **The read chokepoint exists and is sigil-agnostic.** `GetArrayVar` /
  `GetHashVar` (`vm/vm_exec_dispatch.rs:583`, `:705`) begin their resolution
  cascade with `get_env_with_main_alias(name)` (`:626-628`, `:733-760`), whose
  first act is `if let Some(v) = self.unit_scope_lexical(name) { return
  Some(v.into_deref()) }` (`vm/vm_env_helpers.rs:788-810`). A container placed
  in `unit_lexicals` is therefore already reachable from the module's routines
  — the store is checked *before* `env`, which is its entire purpose.
- **The value-replace write chokepoint exists.**
  `set_env_with_main_alias_sym` → `unit_scope_lexical_write`
  (`vm/vm_env_helpers.rs:624-633`) updates the cell in place and reports `true`
  so the bare `env` store is skipped.
- **Container cells are an exercised VM state, not a new one.**
  `box_decl_local_container_cell` (`vm/vm_var_assign_local_get.rs:381-408`)
  already boxes whole `@`/`%` containers into a `ContainerRef` cell for the
  "nested named sub mutates an outer `@names` by name" shape
  (`docs/captured-outer-cell-sharing.md` §7.2), and PR #6711 (`1ec010ba8`,
  2026-08-20) debugged a live production instance of a cell-boxed anonymous
  `@` container in CBOR::Simple. This is ADR-0025 §"Why the skip is obsolete"
  applied to containers: relaxing a skip enters an existing state more often; it
  does not create a new one.
- **A "this name in this frame is a different variable" predicate exists.**
  `container_name_is_redeclared` (`runtime/runtime_shared_vars.rs:238-242`)
  already masks the `__mutsu_atomic_arr::` / `__mutsu_atomic_hash::` lanes for
  exactly this hazard, and is consulted at six sites including
  `get_env_with_main_alias_inner` and `push_to_shared_var`.

## 4. Decision

Adopt the ADR-0024 mechanism for containers, then retire by-name container
resolution outright. Two slices, in this order, for reasons given in §4.3.

### 4.1 Slice 1 — a compunit's `@`/`%` file-scope lexicals get cells

Lift the two `@`/`%` skips that keep containers out of the unit-lexical store,
and fix the *read-miss fallbacks* that the move exposes.

1. **`collect_unit_lexical_names`** (`runtime/run_modules.rs:983-1014`): accept
   a leading `@` or `%` in addition to the current alphabetic-first-char test.
   The surrounding move-into-`unit_lexicals` code (`:824-848`) already wraps the
   value with `into_container_ref` and restores the loading scope's value under
   the plain key, so it needs no change — a `ContainerRef` holding an Array is
   the same shape as one holding a Str.
2. **ADR-0024's mainline capture** (`vm/vm_register_sub_ops.rs:464-470`): drop
   `'@'`/`'%'` from the sigil skip (keep `'&'` — the `&` lane has its own
   registries, per ADR-0025). This is what fixes §1.2's module-free repro, and
   it is the half that makes the fix general rather than module-specific.
3. **Audit and fix the env-miss fallbacks.** This is the real work of slice 1
   and the one place the ticket's warning is still valid. Once a container is
   out of `env`, any site that does `self.env.get(name)` → miss → *builds a
   fresh container and `env.insert`s it* silently drops the mutation into
   storage nobody reads back (`unit_lexicals` is consulted first). The known
   instance is `push_to_shared_var`'s tail
   (`runtime/runtime_thread.rs:929-957`): the `env.get(key)` guard at `:931`
   fails, execution falls to `:950-957`, which builds `Value::real_array(items)`
   from `target_fallback` and inserts it under `key`. The fix shape is
   uniform — resolve through `get_env_with_main_alias` (or a thin
   `unit_lexical_container(name)` accessor returning the cell's container) before
   consulting `env`, and mutate in place via `with_array_inplace` /
   `with_hash_inplace`, which is already write-through.

   **Bound the audit by symptom, not by grep count.** The failing pattern is
   specifically *miss → construct → insert*, not *every* `env` access:
   in-place mutation sites need nothing (§2), and `__mutsu_*` metadata keys are
   not user variables. The inventory to enumerate is "sites that build a
   replacement container when the name is absent from `env`", which is a small
   subset of the ~140 raw accesses.
4. **Whole-container reassignment** (`@arr = <p q>` inside the module) must
   preserve container identity through the cell, i.e. route to
   `cell_store_preserving_container_identity` rather than replacing the cell's
   contents with a detached container — the same in-place-reassign path PR #6711
   corrected for anonymous slots. Watch that PR's hazard: anonymous slot names
   (`@__ANON_ARRAY__` / `%__ANON_HASH__`) are excluded there and must stay
   excluded here.

Explicit exclusions, matching ADR-0024/0025 discipline: `our`, `state`,
`is export`, `$*dynamic`, `::`-qualified, type-constrained containers
(`var_type_constraint`, per `box_decl_local_container_cell:392-401`), and the
anonymous-container names. `our` containers (§1.2's third instance) are a
*separate* fix — the package-qualified mirror already holds the right value, so
they need a resolution change, not a store — and are deliberately out of scope
here.

### 4.2 Slice 2 — containers resolve by slot/upvalue, not by name

Slice 1 makes compunit lexicals safe. It does not make container scoping
lexical: a container declared in an ordinary inner block is still resolved by
name, and every mechanism that had to grow a container special case
(`container_name_is_redeclared`, the atomic-lane masking, the
`module_scope_lexicals` last-resort snapshot) exists only because of that.

Slice 2 closes the §1.3 asymmetry at the compiler:

- `Expr::ArrayVar` / `Expr::HashVar` emit `GetLocal(slot)` when `local_map`
  holds the sigiled name, exactly as `compile_expr_var` does for scalars,
  falling back to `GetArrayVar`/`GetHashVar` only for genuinely free names.
- `compute_upvalues` stops excluding `@`/`%` (`opcode.rs:6124-6130`), so a
  closure or named sub capturing a container gets slot-addressed capture under
  ADR-0018 rather than a bare env name.
- `is_plain_lexical_name`'s `@%&` exclusion (`compiler/mod.rs:1712-1721`) is
  re-examined; the unconditional env mirror on every container `SetLocal`
  (`vm/vm_var_assign_set_local.rs:1863-1878`) is what makes the collision
  possible in the first place.

This is the high-blast-radius half, and it is the one that would let the
container special cases above be *deleted* rather than extended.

### 4.3 Why this order

Slice 2 is the architecturally correct end state, so the ordering needs a
reason. It is not risk aversion:

1. Slice 1 is **mechanism reuse with a known-good precedent** — ADR-0024 ran
   this exact play for scalars, including its failure modes (the seven
   "materially wrong" points in ADR-0024's implementation notes are a ready-made
   checklist). Slice 2 has no precedent in the container lane.
2. Slice 1 **produces the test corpus slice 2 needs.** The pin file
   (§6) is a divergence matrix that must stay green across slice 2's much
   larger change; writing it against slice 1 is how it gets built.
3. Slice 2 changes what a container reference *compiles to* and therefore
   interacts with the ADR-0010 atomic lanes, `shared_vars`, and every by-name
   runtime slot resolver that shadow slots already strain
   (`compiler/mod.rs:1925-1990`'s own doc-comment warning). Doing it after
   compunit lexicals are off the by-name path removes one entire class of
   collision from that change's blast radius.

Slice 1 is **not** a band-aid that makes slice 2 unnecessary: §4.2 states the
end state, and slice 1's exclusion list is the list of things slice 2 must
subsume.

## 5. Alternatives rejected

- **The ticket's `get_container_slot_mut(name) -> &mut Value` canonical-slot
  handle, migrated across ~140 sites.** Rejected: §2 shows the write side is
  already write-through, so the handle would be a mutable resolver for
  mutations that do not need one. It would also be a *new* by-name resolution
  mechanism at a moment when the goal (§4.2) is to have fewer of them, and per
  CLAUDE.md's risk definition a 140-site mechanical migration whose failure mode
  is a quietly-wrong value rather than a red test is the higher-risk route, not
  the lower one.
- **Compile-time alpha-renaming of a compunit's file-scope containers**
  (`@items` → `@UFL::items` at compile time, so all existing `env` sites keep
  working on a name that cannot collide). Attractive — zero runtime resolution,
  zero site migration — but rejected as the primary route: the OTF compile path
  (`vm/vm_call_dispatch.rs:188-198`) builds a fresh `Compiler` with no
  `inherit_enclosing_scopes`, so a sub compiled on the fly would emit the
  *unrenamed* name and silently miss the renamed storage; and symbolic access
  (`::('@items')`, EVAL, interpolation) would need a demangling shim. It fixes
  the module flavour only, leaving §1.2's module-free repro broken. Slice 2 is
  the same idea done properly, at the slot level, where the compiler already has
  the binding.
- **Extending `module_scope_lexicals`** (the existing read-only bare-name
  snapshot, `runtime/mod.rs:1511`, consulted at `vm_exec_dispatch.rs:653` after
  `env`). Rejected for the reason the ticket already gives: being last-resort it
  never fires when a consumer declares the same name — which is this bug's
  precondition — and a snapshot of a mutable container goes stale on the first
  push.
- **Doing nothing until the real-`Test` cutover forces it.** Rejected: §1.2
  shows the bug is not gated on module loading, so "nothing whitelisted depends
  on it today" is a statement about which shapes happen to appear in the
  whitelist, not about exposure.

## 6. Acceptance criteria

- **Pin file: `t/module-file-scope-lexical.t` + `t/lib/UnitFileLexical.rakumod`
  extended with the `@`/`%` cases** that were written and then scoped out of the
  scalar slice (no recoverable git history — `c5bf19e2e` squashed the
  add-and-scope-down — so they are written fresh from this ADR's repros).
- **A divergence matrix pin covering all three instances of the root cause**,
  raku-verified: the module shape (`tmp/ufl/matrix.raku`, 15 assertions over
  read / `push` / element-assign / whole-assign / key-set / `:delete` for both
  `@` and `%`), the module-free mainline shadow shape
  (`tmp/ufl/namedsub-mainline.raku`), and the sub-local consumer shape
  (`tmp/ufl/repro-sub.raku`, whose current failure mode *loses* the module's
  mutation entirely rather than merely misdirecting it).
- The scalar pins stay green: `t/named-sub-lexical-scope.t`,
  `t/for-loop-param-start-sibling-isolation.t`,
  `t/closure-capture-instance-cell.t`, `t/lock-protect-shared-scalar.t`,
  `t/lock.t`.
- **`our @arr` (§1.2) is recorded, not fixed, by slice 1** — a separate ticket,
  since it is a resolution bug against an existing correct store.
- Blast radius is every module with a file-scope container and every named sub
  with a container free variable: full `make roast` delegated to CI, not a
  cherry-picked subset. Watch bench CI after merge (`unit_lexical_slot` gains a
  sigil branch on a hot read path).
- On completion, `git mv` the deep ticket to `news/2026-08/` per the todo
  lifecycle, and update this ADR's Status.

## 6.1 Slice 1 implementation notes (landed 2026-08-20)

The two skips (`collect_unit_lexical_names`, ADR-0024's mainline capture in
`vm_register_sub_ops.rs`) were lifted exactly as §4.1 specified. The read
chokepoint (`get_env_with_main_alias` → `unit_scope_lexical` →
`unit_lexical_slot`) needed no change, as predicted — it already derefs
whatever sigil the stored `ContainerRef` cell holds.

The write-side miss/fallback audit (§4.1 point 3) turned out **wider than the
one call site (`push_to_shared_var`) the ADR named**, because several
independent write chokepoints resolve their target by a raw `env.get(name)` /
`env.get_mut(name)`, each bypassing `unit_lexicals` on its own:

- `push_to_shared_var`'s tail (`runtime/runtime_thread.rs`) — fixed as
  specified: prefer a new `unit_lexical_container(name)` accessor (the
  dereferenced cell contents, sharing the same `Gc`) over the plain-`env`
  fallback, mutating in place.
- `env_root_descended_mut` (`vm/vm_var_assign_index_named.rs`) — this one
  turned out to be the REAL chokepoint: `push`/`pop`/`unshift`/`append`/
  `prepend` (`try_native_array_mut`) and every element-assign site funnel
  through it. Rather than patch each of its ~10 call sites individually, the
  function itself was made to consult a new `unit_lexical_slot_mut` (the
  mutable counterpart of `unit_lexical_slot`, plus a new
  `lookup_in_package_chain_mut` mutable counterpart of the existing
  `lookup_in_package_chain`) FIRST, falling back to `env` only when the name
  is not a unit lexical. This single change fixed element-assign, `push`,
  and hash key-set for free, without touching any of its callers.
- `exec_delete_index_named_op` (`vm/vm_var_delete_ops.rs`, the `:delete`
  handler) does its own independent env-unwrap-and-restore dance (mirroring
  its existing `:=`-bound-cell handling) rather than going through
  `env_root_descended_mut`, so it needed its own fix: seed `env[name]` from
  the unit-lexical cell's contents for the duration of the op, write the
  mutated result back through the cell afterwards, then RESTORE `env[name]`
  to whatever it held before (unlike the `:=`-bound-cell case, which
  deliberately LEAVES the cell installed in `env`) — leaving the cell in
  `env` here would have made the name resolve to the module's container from
  OUTSIDE the module too, undoing the isolation.
- Whole-container reassignment (§4.1 point 4) was already routed through
  `cell_store_preserving_container_identity` by the existing `:=`-bound-cell
  mechanism once the container was cell-boxed; no separate fix was needed
  beyond making sure the anonymous-container exclusion (PR #6711 /
  `1ec010ba8`) stays intact, which it does — `t/anon-container-cell-inplace-reassign.t`
  passes unchanged.

All four fixes carry the same `name.contains("__ANON")` exclusion (or rely on
`is_plain_user_lexical`'s existing exclusion, which already rejects
`__ANON_ARRAY__`/`__ANON_HASH__` since the character after the sigil is `_`,
not lowercase) as defense in depth, even though anonymous-container names are
never actually placed in `unit_lexicals` to begin with
(`collect_unit_lexical_names` and the mainline capture both exclude them at
the source).

**Verification**: the module shape, the mainline-shadow shape, the
sub-local-consumer shape, and `our @arr` (deliberately still broken, per
§4.1's explicit exclusion) were all re-verified against `raku` after the fix.
`t/module-file-scope-lexical.t` grew from 6 to 21 assertions (the original 6
scalar cases plus a 15-assertion `@`/`%` matrix: read / push / element-assign /
whole-assign / key-set / `:delete`, both sigils, both directions — module sees
its own mutation, script's same-named container is untouched). A new
`t/named-sub-lexical-scope-container.t` mirrors `t/named-sub-lexical-scope.t`'s
divergence-matrix rows for `@`/`%` (mainline shadow shape, 8 rows). All scalar
pins listed in §6 stayed green.

## 7. Status of the previously-recorded roast instance

The ticket's measured instance — `roast/integration/99problems-41-to-50.t`
aborting after 1 of 9 assertions with `unknown variable: A` under
`MUTSU_REAL_TEST=1`, blamed on `Test.rakumod`'s `my @vars` colliding with the
test's own `my @vars` — **no longer reproduces**. On `bd34751d3` the file runs
9/9 clean under `MUTSU_REAL_TEST=1`.

The collision *setup* is unchanged (`Test.rakumod:13` still declares `my
@vars`, `:883` still pushes to it; the test still declares `my @vars` at
`:107`), so the file passes for a reason unrelated to this bug being fixed —
most plausibly because the test's `@vars` is method-local and never live across
a `_push_vars` call. Treat it as a stale example, not as evidence of a fix: the
§1.1 and §1.2 repros are the live ones, and §6's matrix replaces it as the
acceptance measure.

## 8. Addendum (2026-08-20): the cross-thread-store axis is the same bug

This section folds the deep ticket
`shared-store-bare-name-collision-across-unrelated-frames` (now retired to
`news/2026-08/`) into this ADR. It adds no new *decision* — §4's decision
already covers it —
but it adds evidence, a third sigil skip to lift, an exclusion §4.1's list
misses, and one requirement slice 2 would otherwise get wrong. It is an
amendment to a `Proposed`, unimplemented design, not a revision of a decided
one.

### 8.1 What the deep ticket claimed, and what is left of it

The ticket's headline was that `shared_vars` is *"a **process-global** map keyed
by bare name"*, so two frames anywhere in the program that happen to use the
same variable name read each other's values, and that the fix is the store's
**keying** — a per-lineage store.

Both halves are now stale:

- **The keying fix already shipped.** ADR-0010 replaced the one process-wide
  `Arc<RwLock<HashMap<..>>>` with the lineage-chained `SharedStore`
  (`src/runtime/shared_store.rs:55-61`, own/parent/root). Sibling isolation
  works: `await (^3).map: -> $n { start { my @w = ($n,); ... } }` gives each
  worker its own `@w` (verified against `raku`).
- **The ticket's own driving instance is gone.** Its multi-param-`for` repro,
  its `while --$i` scalar repro, and the three downstream tickets it named
  (`supply-block-lexical-leaks-through-thread-lane`,
  `cue-loop-lexical-shared-lane-residue`,
  `for-multi-param-array-hash-shadow-clobbers-outer-container`) are all
  resolved; `t/http-session-inmemory.rakutest` is no longer blocked on it.

What survives is narrower and sharper than "the store is bare-name keyed", and
it is not a keying problem at all.

### 8.2 Measured on `52631889f` (2026-08-20)

**Scalars are clean.** Ten shapes were probed — a callee's `while --$i`
countdown, a callee that spawns and *then* writes its `my $i`, `is copy`
parameters, `for`-loop parameters, a Nil-valued reader, a live-valued reader —
and every one matches `raku`.

**Containers diverge, and only when a thread has been spawned.** Two live
shapes:

```raku
# (a) a callee's sub-local container escapes into an unrelated caller
sub work($tag) {
    my @items = ($tag,);
    await start { 1 };          # remove this line and mutsu is correct
    @items.push("$tag-2");
}
my @items = <x y z>;
work('A');  say @items.raku;    # raku: [x y z]        mutsu: [A A-2]
@items.push('MINE');            # raku: [x y z MINE]   mutsu: [A A-2 MINE]
work('B');  say @items.raku;    # raku: [x y z MINE]   mutsu: [B B-2]
```

```raku
# (b) a non-slurpy @/% PARAMETER escapes the call
sub takes(@list is copy) { await start { 1 }; @list.push('R') }
my @list = <x y z>;
takes(<p q>);
say @list.raku;                 # raku: [x y z]        mutsu: [p q R]
```

Both reproduce with `%` identically, and (a) reproduces through a `use`d module
(the module's routine-local `@parts` overwrites the consumer's `@parts`) — the
mirror image of §1.1, where the consumer overwrote the module. Neither needs
concurrency: one `await start { 1 }` anywhere in the process arms the lane, and
the collision is then deterministic and repeats on every call. The `Supply`/tap
driver does **not** arm it; `start`/`Promise` do.

**The scalar/container split is the proof.** The two lanes share the polluted
store; they differ only in *how a read resolves the name*. A scalar reads its
slot (`GetLocal`) and consults the store only when the slot holds `Nil`
(`vm/vm_var_assign_local_get.rs:256,268`). A container has a slot but nothing
reads it (§1.3), so `GetLocal`'s `@`/`%` arm consults the store
**unconditionally** — no `is_thread_clone()` gate, no staleness test
(`vm/vm_var_assign_local_get.rs:155-161`) — and `sync_shared_vars_to_env`
writes every dirty store key straight into `env` under the bare name
(`runtime/runtime_shared_vars.rs:646-648`), where the container read path will
find it. So the store is not the defect; **by-name container resolution is**,
exactly as §1.2 concluded from a module with no threads in it. The store is
simply a second population route into the same by-name namespace.

### 8.3 Why the mask does not save containers: a third sigil skip

§1.3 lists the compile-time sigil exclusions. The thread lane has its own, and
it is what makes §8.2 fire:

- `block_captured_scalars` (`runtime/runtime_thread.rs:20-22`) `continue`s on
  `@`/`%`/`&` when scanning a spawned block's free variables, so a container is
  never in `captured_scalars`.
- `clone_for_thread`'s post-seed retain
  (`runtime/runtime_thread.rs:352-356`) keeps a `thread_redeclared_vars` entry
  only if the name is in `captured_scalars`, `thread_decl_in_flight`, or
  `thread_param_shadow_vars`.

So **every spawn silently unmasks every container `my`**, after which
`set_shared_var_sym`'s write gate (`runtime_shared_vars.rs:495-497`), the
`GetLocal` container arm, and the `sync_shared_vars_to_env` filter (`:587`) all
stop protecting it. `container_name_is_redeclared`
(`runtime_shared_vars.rs:238-242`) — consulted at nine sites specifically to
keep a re-declared container frame-local — is asking a set the spawn just
emptied. This is the same `@`/`%` skip ADR-0024 and ADR-0025 defer and §4.1
step 2 lifts, in a third place.

Repro (b) is a *fourth*: `mask_thread_redeclared_params`
(`runtime_shared_vars.rs:304-311`) deliberately never masks a **non-slurpy**
`@`/`%` parameter, only scalars and `*@`/`*%`. A container parameter therefore
has no per-call shadow at all.

### 8.4 What this adds to §4

1. **Slice 1's exclusion list gains one entry and one non-entry.** Non-slurpy
   `@`/`%` *parameters* (repro (b)) are a distinct binding form from the file-
   scope `my` slice 1 targets; they are **out of scope for slice 1** and belong
   to slice 2, which is where parameters get slots. Record them, do not patch
   `mask_thread_redeclared_params` — widening a bare-name mask is more of the
   mechanism §4.2 is trying to delete.
2. **Slice 2 acquires a hard requirement §4.2 does not state.** Once
   `Expr::ArrayVar` emits `GetLocal(slot)`, the store-writeback path
   (`sync_shared_vars_to_env`, which writes `env` only) can no longer reach the
   reader. The sharing that *must* survive —
   `my @a; await start { @a.push(1) }; say @a` and its `%h` twin, both correct
   today — would silently stop working. The precedent to follow is the scalar
   one already in the tree: `pending_caller_var_writeback` /
   `apply_pending_rw_writeback` (`runtime_shared_vars.rs:652-671`), which drains
   a synced cross-thread name to the owning caller's *slot* at the `await` call
   site. Containers need the same drain, keyed by binding rather than by name.
   **Write the bn9-shaped pin (shared push, shared hash key-set, sibling
   isolation) as part of slice 1's corpus**, per §4.3's second argument, so
   slice 2 cannot regress it unnoticed.
3. **Slice 2's deletion list gains four members.** These exist only because
   containers resolve by name and should be *removed*, not carried forward:
   `container_name_is_redeclared` and its nine call sites; the ungated `@`/`%`
   store preference in `GetLocal` (`vm_var_assign_local_get.rs:155-161`); the
   `is_thread_clone()`-gated twin in `get_env_with_main_alias_inner`
   (`vm_env_helpers.rs:840-846`); and the `@`/`%` exemptions carved into the
   dynamic-variable filters of `clone_for_thread` (`runtime_thread.rs:241-243`)
   and `sync_shared_vars_to_env` (`runtime_shared_vars.rs:600-604`). The
   `__mutsu_atomic_*` lanes are **not** on this list — ADR-0010 established that
   they are process-wide primitives, not lexical sharing, and they stay.
4. **The ticket's proposed fix is rejected outright.** "Re-key the store" is
   both done (ADR-0010) and insufficient: §8.2 (a) collides two frames of one
   thread inside one lineage, so no keying discipline short of per-frame keys —
   i.e. slots — removes it. This is the same conclusion §5 reached about
   compile-time alpha-renaming.

### 8.5 Exposure

No whitelisted roast test and no bundled battery is currently blocked by this;
the ticket's Cro session-test instance was resolved by unrelated fixes and Cro's
own test suite is not vendored. That is a statement about which shapes appear in
the corpus, not about severity: the failure mode is a *silent wrong value* in a
container after any `start`/`Promise` in the process, it repeats on every call,
and any thread-using program with two same-named containers hits it. Treat §8.2
(a) and (b) as acceptance pins for §6 alongside the module and mainline
matrices, and keep the deep ticket open until slice 2 lands — slice 1 alone does
not close it, because §8.2's containers are routine-local, not file-scope.

(Superseded 2026-08-22 by §8.6: the deep ticket closed there instead, because
§8.3's own mechanism turned out to be liftable without waiting for slice 2. The
acceptance pins moved to `t/thread-uncaptured-container-lane.t`.)
### 8.6 What landed (2026-08-22): the lane entry gets a lifetime

§8.4 point 4 concluded that "no keying discipline short of per-frame keys — i.e.
slots — removes it". That is right, and it is also not the only axis available.
A bare-name entry can be wrong in two independent ways: it can name the wrong
binding (a *keying* problem, which needs slots), or it can be **alive when it
should not be**. §8.2 is the second one. `sub work { my @items = ...; await
start { 1 }; ... }` publishes a frame-local container into a process-visible
store because the seeding loop publishes *everything* live at every spawn, and
the entry then outlives the frame it belongs to. Nothing about that requires a
key redesign; it requires the entry to stop existing when it stops being
needed.

So lane entries now have a lifetime:

- `block_referenced_containers` (new, `runtime/runtime_thread.rs`) collects the
  plain-lexical `@`/`%` names in the spawned block's `free_var_syms` /
  `free_var_writes` / `free_var_container_writes` — which already fold up nested
  closures, so `start { start { @a.push(1) } }` counts `@a`. It returns `Option`:
  `None` for the block-less `clone_for_thread` entry point (supply drivers,
  `.then`, socket and proc readers), which keeps its previous behaviour exactly.
- `clone_for_thread_excluding` classifies each entry as it publishes it. A
  container the block **names** is genuinely shared: its entry is durable, and
  any earlier transient mark on it is cleared (promotion). A container the block
  never names, **whose entry this spawn created**, is recorded in
  `transient_lane_containers`. Only entries this spawn created are marked, so an
  entry an earlier naming spawn established stays durable however many unrelated
  spawns walk past it later.
- `sync_shared_vars_to_env` withdraws the marked entries (and their
  `__mutsu_atomic_*` twins, which reads prefer) at the tail of the drain — after
  everything the workers did has been merged back into `env` or written through
  the owning unit-lexical cell. **Only a CLEAN entry is retired.** That
  restriction is what makes the mechanism safe rather than merely narrow: a
  clean entry is positive proof that no thread ever used the lane for that name
  since the spawn published it, which is exactly the "published a binding
  nothing had asked to share" case and nothing else. A dirty entry graduates to
  durable — the moment any thread writes it, the name is in genuine
  cross-thread use, the entry is the authoritative copy, other threads may be
  mid-flight against it, and taking it away underneath them loses updates and
  races the `Gc`.
- Only the **top-level** interpreter classifies. On a worker thread the lane is
  not an optional publication channel a container might or might not need — it
  is the storage: `push @a, ...` routes through `__mutsu_atomic_arr::`
  unconditionally when `is_thread_clone()` (`vm/vm_data_push_ops.rs`), precisely
  so concurrent appends serialize. Retiring an entry there withdraws a
  deliberate mechanism's backing store mid-use (measured: it emptied worker A's
  accumulator in `t/sibling-thread-array-lane-scope.t`), and buys nothing — a
  worker's lineage store is its own (ADR-0010), so its entries cannot outlive
  into an unrelated frame the way a root-store entry published by the main
  interpreter does, which is the collision §8.2 records.

Withdrawing at the drain rather than declining to publish is the load-bearing
choice, and it was arrived at by measurement, not taste — see below.

#### Two cheaper routes, measured and rejected

Both were implemented and run through the full CI suite before this one. The
results are recorded here so the next reader does not re-derive them.

**(1) Decline to seed a container the block never names** — the direct reading
of §8.3's "third `@`/`%` sigil skip". It fixed both §8.2 repros and left the
entire rest of the `t/` suite green (3341 files, 31116 assertions), failing
exactly two assertions, both **indirect**: a worker whose block names only a
routine (`await start { inner('x') }` where `sub inner { @acc.push(...) }`, and
its mainline-named-sub twin) that pushes to an outer container. Those containers
really are shared and the name lane really is what carries them. A static
reachability analysis over the block's own free variables cannot see through a
call, so this route is not merely incomplete — it is the wrong instrument.
Rejected. §8.3's diagnosis of the mechanism stands; its implied remedy does not.

**(2) Resolve the entry by container identity** — replace the
`thread_redeclared_vars` mask in `container_name_is_redeclared` with "is the
store's node the same `Gc` as the one this frame holds?", on the reasoning that
container mutation is write-through-the-shared-node (§2) so a container's node
*is* its binding. Attractive, and it fixed all of the above. But it is unsound
under contention: `Gc::make_mut` inside `shared_array_mutate` reallocates
whenever the node is shared, so a concurrent mutation destroys the very identity
the test depends on, and a reader whose copy has drifted silently decides the
entry is foreign and writes locally. Measured: `t/concurrent-array-index-assign.t`
and `t/concurrent-hash-assign.t` lost updates under heavy contention (20 threads
× 50 indices), `t/escaped-closure-elem-incdec-delete.t` failed, and
`t/cas-multidim-cells.t` timed out. Rejected — and note this is the failure mode
CLAUDE.md's risk definition warns about: correct only under an analysis that is
incomplete in exactly the concurrent case.

#### Relation to §8.4

- **Point 1 respected.** §8.2 (b)'s non-slurpy `@`/`%` parameter is fixed
  without touching `mask_thread_redeclared_params`: the parameter's entry is
  transient and retired at the drain, so it is simply not there when the caller
  reads its own binding afterwards.
- **Point 2 does not arise.** The lane is unchanged for every container a block
  names, so the `pending_caller_var_writeback` drain that requirement protects
  is untouched — indeed the withdrawal deliberately runs *after* it.
- **Point 3 untouched.** The four by-name mechanisms remain for slice 2.
- **Point 4 respected.** Nothing is re-keyed. The claim it makes — that keying
  alone cannot fix §8.2 — is confirmed; what it did not consider is that the
  entry's *lifetime* is a separate axis, and that is the one this uses.

#### What this does not do

Container scoping is still dynamic in the compiler: `Expr::ArrayVar` still emits
a bare `GetArrayVar(name)` (§1.3). A container reached only indirectly is still
shared by bare name for the life of the spawn, and two frames that are *both*
live and both spawning while sharing a name can still collide inside that
window. Slice 2 remains the end state and subsumes all of it — once a container
resolves through its slot, no entry lifetime needs managing. What changed is
that the recorded, reproducible failure mode is closed and pinned.

## 9. Slice 2, first bullet measured (2026-09-04): the read side alone costs 7 `t/` files, and the reason is §1.3

§4.2's first bullet — "`Expr::ArrayVar` / `Expr::HashVar` emit `GetLocal(slot)`
when `local_map` holds the sigiled name" — was implemented in isolation and
measured, then withdrawn. The point of recording it is that "high blast radius"
was previously an estimate; it is now an enumerated list, and the enumeration
says the read side is *not* where the work is.

**Fallout: 7 files, 12 assertions** out of `prove t/`'s 3642 files / 36919
tests (`t/block-local-my-scope.t` 21+23, `t/buf-and-list-mutators.t` 38,
`t/feed-operators.t` 17+18, `t/parameterized-quanthash-key-typecheck.t` 4+8,
`t/push-inline-array-decl.t` 5, `t/quanthash-element-assign.t` 2-4,
`t/var-decl-constraint-clear.t` 7).

**One of §6's two slice-2 acceptance rows is fixed by the read change alone.**
Row (b) — `my @c = 1; sub g { @c.push(7) }; my $h = { my @c; g(); @c }; $h(); say @c`
— answers raku's `[1 7]` instead of `[]`. It was never a *write* bug: the named
sub's push already reached the mainline slot; the closure's own `my @c` clobbered
`env["@c"]`, and only the final by-name read believed it. Row (a) (a closure's
`@a.push` landing on an inner block's shadow) is untouched, because it is a
write-lane defect.

### Why the 12 failures are all one thing

Every one of them is a store site that leaves the slot and `env` naming
different containers, which a by-name read papered over:

1. **An expression-position container declaration allocates no slot.**
   `compile_expr_stmt`'s `Stmt::VarDecl` arm (`compiler/expr_block.rs`) takes
   `decl_slot` only when the declaration `shadows_outer`; otherwise it emits
   `MarkVarDeclContext; SetGlobal(name)` and the container lives in `env` alone.
   `local_map` is **monotonic in the default build** (`pop_local_scope` is a
   no-op unless `shadow_slots_active()`), so a popped sibling block's `@a` slot
   is still in `local_map` — the read compiled to that stale slot while the
   declaration wrote only `env`. This is `t/block-local-my-scope.t` 21/23,
   `t/feed-operators.t` 17/18, `t/push-inline-array-decl.t` 5,
   `t/buf-and-list-mutators.t` 38.
2. **A genuine same-named shadow gives `code.locals` two entries with one
   name**, and the element-assign lane resolves by name across them.
   `my %h = (1 => "a"); { my %h; %h<s> = "t"; say %h<s> }` compiles to
   `locals: ["%h", "%h"]`, `IndexAssignExprNamed { target_slot: Some(1) }`,
   `GetLocal(1)` — and answers `(Any)`, because the handler is env-centric and
   its baked `target_slot` is deliberately ignored unless
   `shadow_slots_active()` (`vm_var_assign_index_named.rs`: an out-of-range
   baked slot must not silently become "not local here"). The mutation COW'd a
   third container that no slot points at. This is
   `t/var-decl-constraint-clear.t` 7 and the two QuantHash files.

### What this changes about slice 2's plan

- The first bullet is **not independently landable**, and not because it is
  large — it is three lines. It is blocked on the *store* lane, which is the
  actual content of slice 2.
- The blocking dependency is **§1.3 of `docs/lexical-scope-slot-campaign.md`**
  (slot-indexed locals / retiring the by-name resolvers), not something internal
  to this ADR: cause 1 is `local_map`'s monotonicity, cause 2 is duplicate
  `code.locals` names, and both are exactly what §1.3 exists to remove. Slice 2
  should be resourced as part of §1.3, sharing its safety net, rather than as a
  standalone container-only change — which also puts it in the same bucket as
  `todo/tickets/same-named-loop-params-in-one-unit-interfere.md`, whose own
  investigation reached §1.3 from the scalar side.
- **Order within slice 2 is therefore inverted from §4.2's bullet order**: make
  the container *store* sites slot-addressed first (expression-position
  declarations allocate a slot; `IndexAssignExprNamed` honours its baked
  `target_slot`), then flip the read. The read flip is the cheap last step and
  its acceptance is already written (§6's rows (a) and (b), plus the 12
  assertions above as the regression set).

## 10. Slice 2 re-measured and re-withdrawn (2026-09-06); §4.1's exclusion list is already empty

§9 measured §4.2's first bullet (container reads emit `GetLocal(slot)`), found 7
`t/` files / 12 assertions of fallout, withdrew it, and concluded the blocker was
§1.3 of `docs/lexical-scope-slot-campaign.md` — the *store* lane. That diagnosis
was **half right**. Re-doing it end-to-end shows the store lane is a short,
concrete list that can be fixed; what actually blocks the flip is a different
lane the ADR had not enumerated.

### 10.1 The re-measurement (do this before planning any more slice-2 work)

Before writing code, §1.1/§1.2's repros, §6's two acceptance rows, §8.2's two
cross-thread rows, and **one row per §4.1 exclusion-list entry** (`our`, `state`,
`is export`, `$*dynamic`, `::`-qualified, type-constrained, anonymous container)
in BOTH the module file-scope shape and the mainline named-sub shadow shape were
run under `raku` v2026.07 and `target/debug/mutsu` on `2a9e06f91`.

| row | shape | status |
|---|---|---|
| §1.1 | module file-scope `my @`/`my %` | agrees |
| §1.2 | mainline named sub × shadowing block, `@`/`%` | agrees |
| §1.2 3rd | `our @`/`our %`, module and mainline | agrees |
| excl. `state` | module `state @`, mainline `state @` × shadow | agrees |
| excl. `is export` | `our @exp is export` | agrees |
| excl. `$*dynamic` | module routine mutating `@*dyn` under a nested re-declaration | agrees |
| excl. `::`-qualified | `@Q::arr` mutated by a sub, with a same-named block `my @arr` | agrees |
| excl. type-constrained | module `my Int @`/`my Int %`, mainline `my Int @` × shadow | agrees |
| excl. anonymous | module `my $anon = [...]` vs consumer `my $anon` | **diverges** (scalar lane) |
| §8.2 (a)/(b) | cross-thread container escape | agrees (closed by §8.6) |
| §6 row (a) | closure's `@a.push` landing on an inner block's shadow | **diverges** |
| §6 row (b) | closure-local `my @c` emptying the mainline `@c` | **diverges** |

**§4.3's "slice 1's exclusion list is the list of things slice 2 must subsume"
is stale: as measured, that list contains no divergences at all.** Those names
were excluded from slice 1's *store*, and the bugs they used to carry were closed
separately — the `our` container resolution fix (2026-08-23), its scalar twin
(2026-08-25), and §8.6's lane lifetime (2026-08-22). Read the list as "shapes
slice 1 deliberately did not put in `unit_lexicals`", not as open bugs. Anyone
planning slice 2 should stop treating it as a work item.

The one exclusion row that does diverge is a **scalar** lane bug: a module's
`my $anon = [...]` colliding with a consumer's `my $anon` (the binding is
`$`-sigiled; only its value is an Array). It reproduces identically without any
slice-2 change and is context-sensitive rather than reducible to two lines —
tracked in `todo/tickets/module-scalar-held-array-collides-with-caller-my.md`.

### 10.2 The read flip, measured again

The flip (`Expr::ArrayVar`/`Expr::HashVar` emit `GetLocal(slot)` when `local_map`
holds the sigiled name) was implemented and measured on `2a9e06f91`. Fallout on
`prove t/` was **8 files / 14 assertions** — §9's 7 files plus
`t/closure-topic-readonly.t` 12 and `t/push-inline-array-decl.t` 3. Four distinct
defects account for all of them, and each is a store site that leaves the slot and
`env` naming different containers, which the by-name read had been hiding:

1. **An expression-position container declaration allocates no slot.**
   `compile_expr_stmt`'s `Stmt::VarDecl` arm (`compiler/expr_block.rs`) takes a
   `decl_slot` only when the declaration shadows an outer binding; otherwise it
   emits `MarkVarDeclContext; SetGlobal(name)`. `local_map` retains a *popped
   sibling block's* slot for a first declaration, so `{ my @a = 5,7,9 } (my
   @a).push: $_ for ^3` read that stale slot. Fix: any container declaration
   whose name already has a reachable slot takes one. (6 of the 14.)
2. **`try_fast_hash_element_assign` resolves its target by name.**
   `vm/vm_var_assign_element.rs` used `find_local_slot` (`position`), which with
   a same-named shadow (`code.locals == ["%h", "%h"]`) answers the OUTER entry:
   the fast path nil'd and re-seeded the wrong binding while the inner
   declaration's slot never saw the write. §9 attributed this to
   `exec_index_assign_expr_named_op_inner` ignoring its baked `target_slot`; the
   real culprit is this fast path, which is never handed the baked slot at all.
   Fix: thread `target_slot` in and use `resolve_local_slot`. (1 of the 14.)
3. **Whole-container rebuilds replace the node.** The `.map` rw element
   writeback (both the `map` builtin and the `.map` method) and
   `classify`/`categorize`'s `:into` target rebuilt the container and inserted
   the fresh node into `env` under the bare name. (3 of the 14.)
4. **A QuantHash re-tag rebuilds its data node.**
   `register_var_container_type_metadata` (`runtime/runtime_container.rs`) tags
   the env value and re-inserts it; a `Set`/`Bag`/`Mix` embeds its metadata in the
   data node, so tagging builds a NEW node. `my %h is MixHash` therefore left the
   declaring slot on the untagged original, and a later `%h<k> = w` from a nested
   frame mutated a container the slot never saw. (5 of the 14.)

With all four fixed, `prove t/` is fully green under the flip (3709 files, 38033
tests), and §6's acceptance row (b) answers raku's `[1 7]`.

### 10.3 Why it was withdrawn anyway: `make roast` names the real blocker

The full whitelist (release, 1436 files) then failed **4 files**, and they are not
more of the same:

| file | shape | lane |
|---|---|---|
| `S32-list/classify.t` 25-27 | `:into(my %b := BagHash.new)` | fix 3's probe promoted a parent-tier env entry into the frame overlay — a genuine bug in the fix, corrected by probing read-only |
| `S03-metaops/hyper.t` 18-19, 92-93 | `@r»++` with an outer same-named `my @r` | `write_back_hyper_target_var` resolves by `find_local_slot`/`locals_set_by_name` (`position` → the OUTER slot). Same class as fix 2; needs a baked slot on `HyperMethodCall` |
| `S15-nfg/concat-stable.t` 4-7, 11-14, 18-21 | `my @n = @o.shift xx $_` — the `xx` LHS is an anon-sub thunk that mutates `@o` | **write/capture lane** |
| `integration/advent2014-day05.t` 6 | `$supply.act: { @seen[$_] //= $_ }` | **write/capture lane** |

The last two are the blocker, and they are one root cause: **a container mutated
from a nested frame (an anon-sub thunk, a `.act`/`start` handler) propagates to
its owner by NAME only.** The frame doing the mutation has no slot for the name,
so the write lands in `env` and, when the write path *replaces* rather than
mutates the node, the owner's slot goes stale. Today's by-name read hides this
completely.

The obvious repair — cell-box the container at its declaration so both halves
alias one cell — is the route
`docs/captured-outer-cell-sharing.md` §7.1d already records as tried and
rejected: broader `@`/`%` decl-site boxing regressed ~12 files through decont
leaks, which is why `register_container_ref_capture_if_free`
(`compiler/expr_call.rs`) is restricted to plain `$` names to this day. Every
"scalars only (containers share via Arc already)" comment in
`vm/vm_env_helpers.rs`'s boxing helpers rests on the same premise — true for
in-place mutation (§2), false the moment a path replaces the container.

So **the read flip is gated on the write/capture lane, i.e. §4.2's SECOND
bullet, not on §9's store lane.** They cannot be landed in the order §4.2 lists
them, and they cannot be landed independently either.

### 10.4 What shipped from this investigation

Only the part that is correct and pinnable **without** the flip:

- `Interpreter::store_container_preserving_identity` (`vm/vm_var_assign_ops.rs`)
  and its three call sites (the `map` builtin's rw writeback, the `.map` method's
  rw writeback, `classify`/`categorize`'s `:into`). It copies a rebuilt
  container's contents into the existing backing node instead of replacing the
  env entry. Two user-visible bugs fixed: `my $b := @a; map { $_ = 5 }, @a`
  left `$b` at `[1 2 3]`, and `my $f := %into; @src.categorize(..., :into(%into))`
  left `$f` empty. Pin: `t/container-rebuild-preserves-identity.t` (10
  assertions, byte-identical under `raku`).

Held back deliberately, because with by-name reads they are **not observable**
and therefore cannot be pinned (measured: the same probes pass identically with
and without them):

- the expression-position declaration slot (fix 1) — worse, it *changes*
  behaviour for the wrong reason without the flip, since the store then lands in
  a slot nothing reads;
- `try_fast_hash_element_assign`'s baked target slot (fix 2);
- `Value::retag_quanthash_in_place` + `register_var_container_type_metadata`
  (fix 4).

They are described precisely enough in §10.2 to be re-derived; do not re-measure
them from scratch.

### 10.5 The order slice 2 should now take

§9 said "store sites first, read flip last". Correct the middle:

1. **The write/capture lane first** — make a container mutated from a nested
   frame reach its owner's binding, without decl-site boxing (§7.1d). §6
   acceptance row (a) is its acceptance test; `S15-nfg/concat-stable.t`'s `xx`
   thunk and `integration/advent2014-day05.t`'s `.act` handler are two more, and
   both are already in the whitelist so a regression is loud. This is adjacent to
   ADR-0055's closure free-variable work and should be resourced with it.
2. **Then the four store fixes of §10.2** together with the read flip, as one
   change — none of them is separately pinnable.
3. `HyperMethodCall` needs a baked target slot in that same change (the hyper.t
   rows above), joining the S1-S17 slot-bake series in
   `docs/lexical-scope-slot-campaign.md`.
4. §4.2's third bullet (`is_plain_lexical_name`'s `@%&` exclusion) stays
   independent and still open.


## 11. §10.3's blocker is closed (2026-09-06); slice 2 is re-attemptable

§10.3 withdrew the read flip for one reason: `make roast` failed two whitelisted
files (`S15-nfg/concat-stable.t`, `integration/advent2014-day05.t`) on a single
root cause — *a container mutated from a nested frame propagates to its owner by
NAME only*, so a replacing write leaves the owner's slot stale. §6's acceptance
row (a) is the two-line form of it.

**That root cause no longer reproduces.** It was closed by `da8e94252` (ADR-0055
slice 1b, "an escaping container capture the frame cannot vouch for gets a
cell"), which boxes the container into a shared `ContainerRef` cell at its
declaration so a closure holds the *binding* rather than the *name*. Note this
lands *after* §10's measurements, which were all taken on `2a9e06f91`.

Verified by building at `da8e94252^` and running six probe shapes there and at
HEAD:

| shape | `da8e94252^` | HEAD | raku |
|---|---|---|---|
| §6 row (a): `.push` from a nested sub with a live shadow | `inner=[3 9]`, owner `[1 2]` | `inner=[3]`, owner `[1 2 9]` | matches HEAD |
| whole-container replace (`@a = 7,8`) | owner `[1 2]` | owner `[7 8]` | `[7 8]` |
| shrinking `.shift` | owner `[1 2 3]` | owner `[2 3]` | `[2 3]` |
| hash key add | owner `(a)` | owner `(a b)` | `(a b)` |
| hash replace | owner `(a)` | owner `(z)` | `(z)` |
| `.=` rebuild | owner `[3 1 2]` | owner `[1 2 3]` | `[1 2 3]` |

Both named roast files pass on HEAD. Pinned as
`t/nested-frame-container-mutation-reaches-owner.t` so the blocker cannot
silently return, and closed out in
`news/2026-09/nested-frame-container-mutation-reaches-its-owner.md`.

**Consequences for the ADR.** §10.3's conclusion — "the read flip is gated on
the write/capture lane, i.e. §4.2's SECOND bullet, and they cannot be landed in
the order §4.2 lists them" — is spent: the second bullet arrived on its own, from
ADR-0055, and §4.2's first bullet is now unblocked. §4.2's ordering claim stands
as history, not as guidance.

What is *not* claimed here: that the flip will now pass. §10.2's four store-side
defects and §10.3's two remaining roast files (`S32-list/classify.t`,
`S03-metaops/hyper.t`) were also measured on `2a9e06f91` and must be re-measured
before being planned around — the same rule that caught this blocker. The work
item now lives at
`todo/deep/adr0039-slice2-container-reads-compile-to-a-slot.md`, with those
defects carried forward and flagged as pre-`da8e94252` measurements.


## 12. Attempt 3 (2026-09-07): seven repairs, `t/` and roast green, blocked by the battery gate

The read flip was implemented a third time, restricted to **plain user lexicals**
(`is_plain_user_lexical` on the sigiled name). It reached a fully green
`prove t/` (3783 files) and a fully green local `make roast` (1436 files, 218962
tests), and was withdrawn by the bundled-library battery gate — a required CI
check that neither of those covers. Only the part that is independently
observable shipped:
`store_container_preserving_identity`'s Set/Bag/Mix arms (repair 5 below), pinned
in `t/container-rebuild-preserves-identity.t`. The rest, with enough detail to be
re-derived rather than re-measured, is in
`todo/deep/adr0039-slice2-container-reads-compile-to-a-slot.md`.

What belongs in the ADR:

**Re-measuring §10 was decisive, and most of it had moved.** §11's blocker was
gone as predicted and §10.2's defect 3 had shipped. Defects 1 and 2 still
reproduced. Defect 4 reproduced with a *different* root cause. Both of §10.3's
roast rows still reproduced and **both recorded diagnoses were wrong**:
`S32-list/classify.t` is not "a bug in fix 3's own probe" but
`store_container_preserving_identity` having no QuantHash arm, and
`S03-metaops/hyper.t` does **not** need §10.5 point 3's baked `HyperMethodCall`
target slot — writing the rebuilt array through the existing node fixes it and
removes the slot search entirely. Against that, the naive flip's fallout had
GROWN from §10.2's 8 `t/` files to **26**, in two families §10 never enumerated.

**The `is_plain_user_lexical` restriction is not a safety margin, it is a
requirement.** The by-name read's tail is load-bearing for non-plain names: its
`None` arm supplies the empty container that makes `%_` read as `{}` on the fast
method-dispatch path, and its cascade is the only route by which `%!attr`/`%.attr`
reach `self`'s attribute cell and `@*dyn` / `%?RESOURCES` / `::`-qualified names
resolve at all. Those shapes are not lexical bindings of the frame; slice 2 is
about the ones that are. Any future widening must supply their behaviour first.

**§8.4 point 2 is narrower than it predicted, and it is about the CELL.** That
point expected the flip to cut the cross-thread store off from the reader. What
was measured instead: `GetLocal`'s `@`/`%` arms consult the `__mutsu_atomic_*`
lanes and the bare-name store *before* the slot, and when the slot holds the
shared `ContainerRef` cell that `env` still names, that ordering is backwards —
`get_env_with_main_alias` has always preferred the cell (its first act is
`unit_scope_lexical`, before its `_inner`'s lane probe). Gating on "the slot's
cell is still what `env` names" closed all 9 cross-thread `t/` files, and both
neighbouring conditions were measured wrong: "any cell in the slot" breaks
`roast/S32-io/IO-Socket-Async.t` 37, "the unit-lexical cell" excludes the case the
gate exists for. §8.4 point 2's `pending_caller_var_writeback`-shaped drain was
**not** needed.

**The remaining blocker is the `@` half of that same cell story, in `gather`.**
`shared_hash_elem_set` writes its merged hash through the binding's cell
unconditionally; `shared_array_mutate` does so only on its non-`is_thread_clone`
branch. A container appended to from inside a `gather`-driven frame therefore
ends up in the atomic lane with the owner's cell left empty — invisible while the
owner re-resolves the NAME (the lane wins that cascade), wrong the moment it
reads its slot. The reduction that found this is worth reusing: a temporary
compiler env-var restricting the flip to a name list, plus delta debugging over
the names a run touches, reduced a 3000-line vendored library to one variable.

**Container identity is the repair for three of the seven defects, and it
generalises.** `store_container_preserving_identity` (§10.4) turned out to be the
right primitive well beyond its original three call sites. The rule that keeps
falling out is §2/§3's: a runtime helper that REBUILDS a variable's container must
copy the result into the existing backing node, never drop a fresh node into
`env`; and every by-name slot search such a helper uses to "also update the slot"
(`locals_set_by_name`, `find_local_slot`) is both unnecessary and wrong under a
shadow.

**Process note for attempt 4.** `t/` and `make roast` are not sufficient evidence
for this change — both were green. `scripts/battery-testsuite.sh` is, and it runs
locally in about ten minutes.
