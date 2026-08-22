# ADR-0039: `@`/`%` lexicals must resolve lexically — retiring by-name container resolution, and correcting the "container write-back is structurally different" premise

- Status: Slice 1 (§4.1) landed 2026-08-20. The cross-thread axis (§8) closed
  2026-08-22 via binding identity rather than a bare-name mask — see §8.6.
  Slice 2 (§4.2 —
  containers resolve by slot/upvalue, not by name, retiring the by-name path at
  the compiler) is next, and is now purely a mechanism-deletion slice (§8.4
  point 3) rather than a correctness one.
- Date: 2026-08-20
- Related: ADR-0013 (container interior mutability — `gc_contents_mut`),
  ADR-0024 (mainline lexicals for named subs — the scalar half of this bug),
  ADR-0025 (cell boxing must be value-kind-blind — slice 3 defers `@`/`%`),
  ADR-0010 (cross-thread lexical sharing scope — the `__mutsu_atomic_*` lanes)
- Addresses: `todo/deep/module-file-scope-array-and-hash-still-share-the-caller.md`,
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

### 8.6 What landed (2026-08-22): binding identity instead of a bare-name mask

§8.4 point 4 concluded that "no keying discipline short of per-frame keys — i.e.
slots — removes it", and that is right as far as it goes. What it misses is that
for a **container**, per-frame identity is available without waiting for slots:
container mutation in mutsu is write-through-the-shared-node (§2 / ADR-0013 §7),
so a container's `Gc` node **is** its binding. Slice 2 would read that identity
off the frame; this reads it off the value.

`container_name_is_redeclared` (`runtime/runtime_shared_vars.rs`) is the
predicate all nine lane gates already consult. It used to answer only "was this
name masked by a `my` since the last spawn?", which is structurally incapable of
covering §8.2:

- the mask is populated only while `shared_vars_active`, so the callee's
  `my @items` — which runs *before* the process's first spawn — is never masked
  at all; and
- the mask is not scoped to the declaring frame, so even when it is set it is
  dropped at the next spawn (§8.3) and outlives the frame when it is not.

It now also answers **"is the store's entry under this name a different
container from the one this frame holds?"**
(`container_store_binding_is_foreign`): resolve the name the way the frame would
without the store (`unit_lexicals` first, then `env`), and compare its `Gc` node
against the store's base entry and its authoritative `__mutsu_atomic_*` copy. A
match means the entry is about *this* binding and every lane preference is
correct; no match means it belongs to another frame and this one stays local.

It is conservative in the direction that preserves sharing: no live local
binding, a non-container value, or a name absent from the store all answer
"not foreign", leaving the existing behaviour exactly as it was. The check is
restricted to plain lexical names, so twigil'd, dynamic (`@*x`), attribute and
`::`-qualified containers keep their own routes untouched, and it only runs
while `shared_vars_active`.

Why this and not the seeding restriction it replaced: the first attempt declined
to *publish* a container the spawned block never names — the direct reading of
§8.3's "third sigil skip". It fixed both §8.2 repros and left the entire rest of
the `t/` suite green, but broke exactly two shapes, both **indirect**: a worker
whose block names only a routine (`start { inner('x') }`) that pushes to an
outer container. Those containers really are shared, and the name lane really is
what carries them, so a static reachability analysis over the block's free
variables is the wrong instrument — it cannot see through a call. The identity
test needs no reachability analysis: in the indirect shapes the store's entry
*is* the frame's own container, so it answers "not foreign" and the sharing
stands.

**Relation to §8.4.** Point 1 is respected: §8.2 (b)'s non-slurpy `@`/`%`
parameter is fixed without touching `mask_thread_redeclared_params` — the
parameter's container is simply not the one the store holds. Point 2's drain
requirement does not arise: the lane is preserved wherever the store's entry is
this binding, so the writeback path it protects is unchanged. Point 3 is
untouched and still belongs to slice 2. Point 4 is respected in substance —
nothing is re-keyed, and the identity test is not a keying discipline.

**What this does not do.** Container scoping is still dynamic in the compiler:
`Expr::ArrayVar` still emits a bare `GetArrayVar(name)` (§1.3), and the four
by-name mechanisms in §8.4 point 3 are still there — `container_name_is_redeclared`
in particular is now *more* capable rather than deleted. Slice 2 remains the end
state, and subsumes this: once a container resolves through its slot, the
identity question never needs asking. What changed is that slice 2 is now a
mechanism-deletion slice rather than a correctness one.
