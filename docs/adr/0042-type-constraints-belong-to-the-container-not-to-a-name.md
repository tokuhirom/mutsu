# ADR-0042: A type constraint belongs to the container, not to a name — retiring the `var_type_constraints` side table

- Status: Partially Implemented — Slice 1 landed 2026-08-20 (see §10); its
  follow-on "outer-first shadow" finding fixed 2026-08-22 (see §11, which
  supersedes part of §5.2/§6). Slices 2 and 3 not started.
- Date: 2026-08-20
- Related: ADR-0013 (container interior mutability), ADR-0024 (mainline lexicals —
  the same by-name/lexical split for scalar *values*),
  ADR-0025 (cell boxing must be value-kind-blind),
  ADR-0039 (`@`/`%` lexicals resolve lexically — §7 below shows this ADR owns
  ADR-0039 §4.1's typed-container exclusion)
- Addresses: `todo/deep/bare-name-type-constraint-store-is-scope-blind.md`

## 1. Context

`Interpreter::var_type_constraints` (`src/runtime/mod.rs:1803`) is a
`HashMap<String, String>` keyed by BARE variable name and never frame-scoped.
A `my Int @a` anywhere in the process registers `"@a" -> "Int"` globally, and
any later `@a` — a different variable, in a different frame, in a different
file — is type-checked against it.

Two slices of this were fixed on 2026-08-13 and recorded in the deep ticket:
routine-scoped typed **scalars** (`OpCode::SetVarTypeScoped`, registered
env-scoped only, `news/2026-08/typed-lexical-constraint-frame-scoped.md`), and
typed **scalars** in a genuine source `{ ... }` block
(`Compiler::lexically_in_block`, pinned by
`t/typed-lexical-constraint-block-scoped.t`).

The ticket then described the remainder as narrow — "containers and mainline
blocks only", plus two named compile paths. **That scoping is wrong in one
direction and, more importantly, wrong about the nature of the container half.**
This ADR corrects both and decides the end state.

## 2. Measured on `3766df1de` (2026-08-20)

Every result below is a matrix run under `raku` and `target/debug/mutsu`, where
each row declares a **typed** variable in some inner scope and then, after that
scope has exited, uses a same-named **untyped** outer variable. `raku` accepts
every row; a mutsu rejection is a leaked constraint.

### 2.1 Scalars — seven shapes still leak, not two

`tmp/tc14-scalar-matrix.raku`:

| inner scope declaring `my Str $x` | mutsu |
|---|---|
| routine body | ok (fixed) |
| genuine `{ ... }` block | ok (fixed) |
| statement-modifier `if` inside a block | ok (fixed) |
| `if` branch | **LEAK** |
| `unless` branch | **LEAK** |
| `else` branch | **LEAK** |
| `while` body | **LEAK** |
| C-style `loop` body | **LEAK** |
| `repeat` body | **LEAK** |
| `for` body | **LEAK** |

The ticket named `if`/`while`/`for`/C-style. `unless`, `else` and `repeat` are
also live, and the two compile paths it named do not cover all seven: loop
bodies take `compile_body_with_implicit_try`
(`src/compiler/helpers_control_flow.rs:563-565`), which emits **no scope
wrapper of any kind** — no `BlockScope`, no `BlockLocalScope` — so
`lexically_in_block` is false and `emit_set_var_type` picks the global
`SetVarType`. Branch bodies take `compile_block_local_branch` (`:482-491`) →
`OpCode::BlockLocalScope`, whose exit cleanup
(`src/vm/vm_control_ops.rs:328-349`) removes only the **bare** name key via
`remove_sym(*sym)` and never `__mutsu_type::<sym>`.

### 2.2 Containers — every shape leaks

`tmp/tc13-container-matrix.raku`: a typed container declared in a routine, a
bare block, an `if` branch, a `while` body, a `for` body — plus `my Int %h` and
`my %h{Int}` — leaks in **7 of 7** shapes, including the two shapes already
fixed for scalars. Containers were never in scope for either 2026-08-13 fix:
`Compiler::emit_set_var_type` (`src/compiler/mod.rs:2191-2206`) excludes them
from the scoped opcode by sigil, verbatim:

```rust
let scoped = !is_our
    && (self.is_routine || self.lexically_in_routine || self.lexically_in_block)
    && !name.starts_with('@')
    && !name.starts_with('%')
    && !name.starts_with('&')
    ...
```

Its own doc comment gives the reason: containers keep the both-store form
*because* their element/key metadata is read through the global map by the
push/subscript fast paths.

## 3. The premise this ADR corrects

The ticket calls the container residual "the meaningful one" and sizes it as
architectural work:

> The remaining fix direction for the residuals is the same as the ticket
> originally proposed — carry constraints on the container/cell (ArrayData/
> HashData already carry element types; scalars would need cell-carried `of`)
> and make the name-keyed store compile-time/EVAL bridging only. The container
> residual (1) is the meaningful one.

**For containers, that work is already done and already correct.** The
constraint is carried on the value: `ArrayData` and `HashData` both hold
`value_type` / `key_type` / `declared_type` (`src/value/value_collections.rs:43-166`).

The decisive measurement is `tmp/tc16-alias-probe.raku`. For each typed-container
shape it binds a **differently-named** alias (`my @x1 := @a1`) and pushes a bad
element through the alias. Enforcement through a different name cannot come from
the bare-name map — only from the container. Result: **8 of 8 shapes enforced**,
matching `raku` exactly, across fresh declaration, initializer, whole-assign,
post-push, `my Int %h`, `my %h{Int}`, and shaped `my Int @s[3]`.

So for containers the name map is not the mechanism. It is a **redundant second
source of truth that contributes only false positives** — precisely the leaks in
§2.2. The container-first accessor even exists already,
`Interpreter::element_constraint_for` (`src/runtime/runtime_container.rs:337-345`),
and its doc comment states this ADR's thesis:

> Element type constraint for a container variable, preferring the metadata
> embedded in the value itself over the name-keyed `var_type_constraints` side
> table. The embedded metadata travels with the value through frame
> save/restore, so it stays correct when a recursive call re-binds a same-named
> variable to a differently-typed container — the name-keyed store is clobbered
> by the inner frame in that case.

The thirteen `var_type_constraint_fast` call sites simply do not use it.

**Scalars are the opposite case, and are the genuinely architectural half.**
`ValueRepr::ContainerRef(Gc<Mutex<Value>>)` (`src/value/mod.rs:1301`) is a bare
cell with no constraint field, so a typed scalar's `of` has nowhere to live on
the value. The same alias probe diverges:

```raku
my Str $s; my $t := $s; $t = 42;   # raku: dies    mutsu: assigns
```

The ticket's sentence is therefore exactly inverted: the container half is the
*mechanical* one, and the scalar cell-carried `of` is the deep one.

### 3.1 The one container gap

`state` containers are the single measured shape whose constraint is *not* on
the value: `state Int @a; my @x := @a; @x.push("s")` is enforced by `raku` and
not by mutsu, while the direct `state Int @a; @a.push("s")` is enforced by both
— i.e. today only the name map enforces it. This is a live divergence in its own
right and a prerequisite for slice 1.

## 4. The circular dependency

Containers stay on the global map because the fast paths read the global map;
the fast paths read the global map because it is the only thing they were given.
Breaking the cycle at the read sites — not at the declaration sites — is what
makes the rest fall out.

The thirteen map-only readers (`var_type_constraint_fast`,
`src/runtime/runtime_var_meta.rs:285-290`) are:

| Site | Operation |
|---|---|
| `vm_data_push_ops.rs:18` | `push_nil_to_elem_default` |
| `vm_data_push_ops.rs:312` | `check_push_element_type` |
| `vm_data_push_ops.rs:362` | `native_int_element_constraint` |
| `vm_var_assign_element.rs:56` | shared-hash element assign bailout |
| `vm_var_assign_element.rs:131` | shared-array element assign bailout |
| `vm_var_assign_element.rs:235` | fast hash element assign bailout |
| `vm_var_delete_ops.rs:113` | `:delete` bailout |
| `vm_misc_assign.rs:73` | whole-`%h` reassign Mix-trait RO check |
| `vm_var_assign_coerce.rs:162` | QuantHash coercion on hash store |
| `vm_exec_dispatch.rs:537` | `GetGlobal` Nil → type object |
| `vm_exec_dispatch.rs:884` | `SetGlobal` `__ANON_STATE__` guard |
| `vm_var_assign_local_get.rs:302` | `GetLocal` Nil → type object |

The two Nil→type-object readers are deliberately map-only and documented as
such ("an env-scoped constraint must not turn a genuinely-Nil read into the type
object"); they are scalar reads and are **out of scope** for slice 1.

## 5. Decision

The constraint's home is the container. The name-keyed map is demoted to a
declaration-time/EVAL bridge and then deleted. Three slices.

### 5.1 Slice 1 — container constraints are read from the container

Mechanical, and ready for direct implementation. No design work remains.

1. Route the ten **container** sites in §4's table through the value-carrying
   accessor (`element_constraint_for` / `container_type_metadata`) instead of
   `var_type_constraint_fast`. Each already has the receiver value in hand or
   one `env` read away — these are the sites that resolve a container in order
   to mutate it.
2. Close §3.1: make a `state` container's declaration tag the value with its
   `ContainerTypeInfo`, the way `my` does via
   `register_var_container_type_metadata` (`runtime_container.rs:275-288`).
3. Drop `'@'`/`'%'` from `emit_set_var_type`'s sigil exclusion
   (`src/compiler/mod.rs:2191-2206`), so a typed container declared inside a
   routine or block registers env-scoped like a scalar does. Keep `'&'`.
4. Teach `exec_block_local_scope_op`'s exit cleanup
   (`src/vm/vm_control_ops.rs:328-349`) to also remove
   `__mutsu_type::<sym>` / `__mutsu_hash_key_type::<sym>`, mirroring the
   prefix-stripping `exec_block_scope_op` already does
   (`src/vm/vm_misc_scope.rs:585-595`).

After slice 1, §2.2's matrix goes green and §2.1's `if`/`unless`/`else` rows go
green (step 4 covers `BlockLocalScope`).

### 5.2 Slice 2 — a scalar cell carries its `of`

The architectural half. Give the scalar container a constraint field so a typed
scalar's `of` travels with the container exactly as `ArrayData::value_type` does
— i.e. `ContainerRef` stops being a bare `Gc<Mutex<Value>>`. This is what makes
`my Str $s; my $t := $s; $t = 42` die (§3), and it is the precondition for
retiring the env-scoped `__mutsu_type::` lane as well as the global map.

It also removes two guards that exist *only* because a constrained scalar must
not be cell-boxed: `box_decl_local_cell`'s constraint bail
(`src/vm/vm_var_assign_local_get.rs:358-362`) and its container sibling
`box_decl_local_container_cell` (`:392-401`). Both probe **env-first by bare
name**, so a stale global entry from an unrelated scope silently disables
cell-boxing for a same-named variable today — a latent interaction with
ADR-0025 and ADR-0039 that slice 2 deletes rather than patches.

The remaining §2.1 rows (`while`, C-style `loop`, `repeat`, `for` bodies) are
closed here rather than by growing `lexically_in_block`: those bodies have no
scope-boundary opcode to hook (§2.1), and adding one purely to carry a metadata
key would be more of the mechanism this ADR deletes. A cell-carried `of` needs
no scope boundary at all.

### 5.3 Slice 3 — delete the map and its workarounds

With both sigil lanes reading the value, `var_type_constraints` retains only its
declaration-time and EVAL-bridging role, and these workarounds — which exist
solely because the map is not scoped — are **deleted, not carried forward**:

- `vm_for_loop_body.rs:300-327` / `:1029-1035`, the multi-parameter `for`-loop
  save/clear/restore of the global map. Its comment names the motive exactly:
  *"That map is not block-scoped, so an unrelated `my Int $v` anywhere in the
  program made `-> $k, $v` reject every non-Int value."* It also has a latent
  defect to retire with it: the save reads env-first (`var_type_constraint`)
  but the restore writes **both** stores, promoting an env-only constraint into
  the global map.
- `test_functions/throws_like.rs:113-114`, which folds the env-first effective
  constraint into a nested interpreter's map specifically because
  `var_type_constraint_fast` never consults env.
- `test_functions/tap_subtest.rs:29,47,64,88`, the whole-map snapshot/restore
  around every `subtest`.
- `bind_param_type_constraint`'s global CLEAR
  (`src/runtime/runtime_var_meta.rs:260-261`) and
  `set_var_type_constraint_impl`'s (`:172`), which are the ticket's residual 3:
  an untyped inner declaration deleting an outer scope's entry. Measured today
  on containers — `my Int @a` outer, untyped `my @a` in a routine, and the
  outer array stops being enforced after the call (`tmp/tc6`). Once enforcement
  reads the container this is structurally impossible.

## 6. Alternatives rejected

- **Keep extending `lexically_in_block` / the scoped opcode, one compile path at
  a time.** This is the status quo's trajectory: two paths fixed, at least five
  scalar paths and the whole container lane left. §2.1 shows the enumeration was
  already wrong once (it missed `unless`/`else`/`repeat`). Each extension needs
  its own VM-side cleanup because each path has a different scope mechanism (or
  none), and none of it helps containers. Per CLAUDE.md's risk definition, a
  fix whose completeness depends on having enumerated every compile path is the
  higher-risk route.
- **Give loop and branch bodies a real scope-boundary opcode so the existing
  mechanism applies uniformly.** Rejected as the primary route: it adds a
  boundary on the hottest paths in the VM to carry a metadata key that, per §3,
  should not be name-keyed at all. Slice 2 removes the need.
- **Snapshot `__mutsu_type::<name>` into `loop_local_saved_env`** (the ticket's
  suggested shape for the `BlockLocalScope` path). It is a correct local patch
  and slice 1 step 4 does the equivalent for branches, but it is not a fix: it
  covers neither the no-wrapper loop bodies nor any container, and it grows the
  by-name mechanism.
- **Make `var_type_constraint_fast` env-first.** That is just `var_type_constraint`,
  whose `format!` + env probe on the push/subscript hot path is why the fast
  variant exists. Reading the receiver's own metadata is both correct and
  cheaper than a string-keyed env probe.

## 7. Relationship to ADR-0039

ADR-0039 §4.1 lists its slice-1 exclusions, and one of them is
**type-constrained containers** (`var_type_constraint`, citing
`box_decl_local_container_cell:392-401`). That exclusion exists precisely
because a typed container's constraint is name-keyed and must keep flowing
through the assignment chokepoint. **This ADR owns that exclusion**: slice 1
here is what lets ADR-0039 lift it, and slice 2 here is what deletes the
`box_decl_local_container_cell` guard ADR-0039 cites.

The two ADRs are complementary, not overlapping. ADR-0039 moves a container's
**value** off by-name resolution; this ADR moves a container's **type
constraint** off by-name resolution. Both are instances of ADR-0024's thesis,
and the precedent for the retirement is already in the tree — the
`hash_object_keys` side table was retired exactly this way
(`src/runtime/runtime_container.rs:257-260`: *"the `hash_object_keys` side table
and its pointer-migration helpers are gone — the map now travels with the hash
across copy-on-write"*), as was `hash_type_metadata`
(`src/value/value_collections.rs:61-64`).

Sequencing: slice 1 here is independent of ADR-0039 slice 1 (in flight as
PR #6729) and can land in either order; they touch different files. ADR-0039
slice 2 and this ADR's slice 2 both change what a container/scalar reference
compiles to and should not be in flight simultaneously.

## 8. Acceptance criteria

- **Pin the two matrices verbatim**, `raku`-verified: `t/typed-constraint-scope-matrix.t`
  covering §2.1's ten scalar shapes and §2.2's seven container shapes.
- **Pin the alias probe** (§3): for every typed-container declaration shape, a
  differently-named bound alias still enforces — this is what proves enforcement
  reads the container and not the name, and it must stay green across slice 3's
  deletions.
- `state Int @a` through an alias enforces (§3.1).
- After slice 2: `my Str $s; my $t := $s; $t = 42` dies.
- The existing pins stay green and their scope notes are updated:
  `t/typed-lexical-constraint-frame-scoped.t`,
  `t/typed-lexical-constraint-block-scoped.t` (whose header documents the
  remaining gap and must be rewritten as each slice lands),
  `t/typed-array-push-typecheck.t`, `t/typed-array-mutate-preserves-type.t`,
  `t/typed-array-alias-reassign.t`, `t/typed-array-nil-default.t`,
  `t/state-typed-scalar.t`, `t/cas-typed-constraint.t`.
- Blast radius is every typed declaration in the language: full `make roast`
  delegated to CI. Watch bench CI after slice 1 — `check_push_element_type` is
  on the `@a.push` hot path and swaps a map probe for a container-metadata read.
- On completion, `git mv todo/deep/bare-name-type-constraint-store-is-scope-blind.md`
  to `news/2026-08/` and update this ADR's Status.

## 9. Status of the ticket's residual 4

The ticket's residual 4 — "`for`-loop typed params still save/restore the global
map by name" — **does not reproduce as a divergence** on `3766df1de`. Probed:
an outer typed lexical shadowed by a differently-typed `for` parameter (scalar
and container), a typed `for` parameter shadowing a name used in a callee,
early exit from the loop body via `last` / `die` / `return`, and the env-only→
global promotion of §5.3. All match `raku`.

It is therefore not a live bug but a **mechanism that should not exist**: the
save/restore is a workaround for the unscoped map, carrying the latent
promotion defect noted in §5.3. It is recorded here as part of slice 3's
deletion list, not as a failing shape to fix.

## 10. Slice 1 status (landed 2026-08-20)

All four §5.1 steps landed as specified, with two adjustments discovered
during implementation:

- **Extra fix, not in the original four steps:** `set_var_type_constraint_routine_scoped`
  (`src/runtime/runtime_var_meta.rs`) only ever registered the env-scoped
  `__mutsu_type::name` (value type) entry, never `__mutsu_hash_key_type::name`
  (key type). Step 3 (dropping `%` from `emit_set_var_type`'s sigil
  exclusion) routes a key-only object hash declared inside a routine/block
  (`my %h{Int}`, empty `value_type`) through this SCOPED path for the first
  time — and losing the key-type registration there silently dropped key-type
  enforcement for exactly that shape (measured: `sub f { my %h{Int}; %h{1} =
  "a"; %h{"bad"} = "b" }` stopped dying). Fixed by also registering
  `__mutsu_hash_key_type::name` there. A second, related fix: three
  `var_type_constraint_fast(name).is_some() || ... || var_hash_key_constraint_fast(name)`
  bailout checks (`try_shared_hash_element_assign`, `try_fast_hash_element_assign`,
  `try_fast_hash_delete`) were routed through a single
  `container_type_metadata(&current).is_some()` check instead of
  `element_constraint_for` — `element_constraint_for` filters out an empty
  `value_type`, which is exactly what a key-only object hash has, so it alone
  would have reintroduced the same gap at the read side. `container_type_metadata`
  (true iff `value_type` OR `key_type` OR `declared_type` is set) does not.
  `var_hash_key_constraint_fast` is now dead code (no remaining callers) and
  was removed.
- **One step (a §5.1-step-4 extension) was prototyped and reverted.** An
  attempt to also fix the "outer-first shadow" shape (see the new finding
  below) by extending `loop_local_saved_env` (the mechanism
  `pop_loop_local_scope` already uses to restore a shadowed outer binding's
  bare-name value) to snapshot/restore the `__mutsu_type::`/
  `__mutsu_hash_key_type::` metadata keys too was implemented and measured to
  have **no effect** on that shape — proving its root cause is at the VALUE
  layer (the container's own embedded metadata gets corrupted), not the
  name-keyed metadata layer this ADR's slice 1 targets. Reverted to keep
  slice 1 scoped to its four specified, verified-effective steps.

**Verified green**, `raku`-oracled: the §2.2 container matrix (all 7 shapes:
routine/block/if/while/for/`my Int %h`/`my %h{Int}`), the §2.1 `if`/`unless`/
`else` scalar rows, the §3 alias probe (7/7 shapes), and the §3.1 `state`
container gap. Pinned in `t/typed-constraint-scope-matrix.t` and
`t/state-typed-container-alias.t`. Zero regressions across the 62 pre-existing
`t/*typed*` files and the `S02-types`/`S09-typed-arrays` roast whitelist
(19 files, 4824 tests).

**New finding, NOT fixed by slice 1 and NOT one of its four steps:** a
DIFFERENT, pre-existing bug — present on `main` before slice 1 and unaffected
by it — where a typed declaration that SHADOWS an already-existing outer
binding of the same name (as opposed to being a fresh, non-shadowing
declaration) leaks its constraint onto that outer binding, for BOTH scalars
and containers, across EVERY branch/loop construct measured —
`if`/`unless`/`else` are affected exactly as much as `while`/`loop`/`repeat`/
`for`, contradicting this ADR's own §2.1 prediction that step 4 would fix the
former. **Fixed 2026-08-22 — see §11.**

## 11. The "outer-first shadow" shape (fixed 2026-08-22)

§10's finding is closed. `news/2026-08/typed-declaration-shadow-scope-leak.md`
has the full write-up; three things in this ADR were wrong and are corrected
here.

**It was a name-layer bug, not a value-layer one.** The ticket concluded that
`tag_container_metadata`'s copy-on-write corrupts the shadowed outer
container's own embedded metadata in place. The alias probe of §3 disproves
that directly: after the leak, `my @z := @a; @z.push("x")` SUCCEEDS while
`@a.push("x")` dies — enforcement a different name escapes is not coming from
the container. The leak was the name-keyed `__mutsu_type::<name>` env entry
throughout.

**Why §10's prototype measured "no effect", and why that inference was wrong.**
Extending `loop_local_saved_env` was the right mechanism in the wrong place.
The compiler emits the type-constraint op BEFORE the declaration's own
`SetLocalDecl` store, so hooking the store saves an already-clobbered value.
The fix records the pre-declaration metadata inside `exec_set_var_type` itself
(`Interpreter::save_type_meta_for_scope_exit`) — the one point where the old
value is still readable — and lets the existing `pop_loop_local_scope` restore
it. That one hook serves branches and every loop form, because all of them
already bracket their body with `push_loop_local_scope`/`pop_loop_local_scope`.

**§5.2's assignment of the remaining §2.1 rows is superseded.** Those rows
(`while`, C-style `loop`, `repeat`, `for` bodies) do not need slice 2's
cell-carried `of`, and §6's rejection of "keep extending `lexically_in_block`"
rested on the premise that "each path has a different scope mechanism (or
none)" — which the uniform `loop_local_saved_env` hook removes. With the VM
restore in place, `lexically_in_block` is now set while compiling a
`BlockLocalScope` branch body and every loop body, so a MAINLINE typed
declaration in one uses the env-only scoped opcode instead of also writing the
global map. Inside a routine this changes nothing (already scoped via
`is_routine`); it is purely what extends the fix to mainline. `our`, `&`,
dynamics, `__ANON_STATE__` and package-qualified names keep the both-store
opcode, so the exclusions §5.1 relies on are untouched.

**Residual, and it IS slice 2's.** A typed outer SCALAR still loses its
constraint once any inner declaration of the same name — typed *or* untyped —
has shadowed it in a branch/loop body. It reproduces identically before and
after this fix and is scalar-only, precisely because a container carries its
constraint on the value and a scalar has nowhere to put one (§3). Tracked in
`todo/deep/shadowing-declaration-drops-the-outer-typed-scalar-constraint.md`
and pinned as three `# TODO` rows in `t/typed-constraint-shadow-scope.t`.

Pinned by `t/typed-constraint-shadow-scope.t` (35 `raku`-verified assertions,
replacing the expected-failing `t/typed-constraint-shadow-leak-unfixed.t`).
