# ADR-14632: An element store into a local the code declares operates on the slot, not on the name

- **Status**: Proposed (awaiting the maintainer's Accept)
- **Date**: 2026-10-23
- **Issue**: [#14632](https://github.com/tokuhirom/mutsu/issues/14632) (retire the env-centric
  named element-store handler), which also carries
  [#14139](https://github.com/tokuhirom/mutsu/issues/14139)'s close condition.
- **Related**:
  [ADR-13757](13757-a-locals-type-constraint-comes-from-its-code.md) (a local's constraint
  comes from its code; this ADR applies the same rule to the container itself),
  [ADR-0097](0097-a-binding-descriptor-addressed-by-slot.md) (`BindingDesc`; §15's
  "divergence is an invariant violation"),
  [ADR-0039](0039-container-lexicals-resolve-lexically.md) (§4.2: container reads compile to
  `GetLocal`; the unconditional container env mirror is its third bullet),
  [ADR-12529](12529-frames-resolve-names-through-the-static-link.md) and
  [ADR-14633](14633-call-frame-materializes-no-name-keyed-env.md) (frames without a
  name-keyed env), ADR-0068 (cross-thread container stores),
  [#11510](https://github.com/tokuhirom/mutsu/issues/11510) (slow-path retirement).

## 1. Context

### 1.1 What the named element store does today

`@a[$i] = $v`, `%h<k> = $v` and their slice, compound and `:=` forms compile to
`OpCode::IndexAssignExprNamed { name_idx, target_slot, .. }`. Its handler
(`exec_index_assign_expr_named_op_inner`, `src/vm/vm_var_assign_index_named.rs`, about 3,000
of that file's 6,300 lines) treats the variable's **name** as the location:

1. It probes the env for a sigilless-alias record under the name and, if one exists, stores
   into the variable the alias names instead.
2. It finds the container through `env_root_descended_mut_tracked(name)`, a seven-way
   precedence: an unmerged package's `our` key, the compunit's unit lexicals, the running
   package's `our` mirror, the `our` scalar cell, a package alias (`constant E = A::B`), the
   package-block lexicals, and finally `env.get_mut(name)`. A `ContainerRef` cell found there
   is descended.
3. About fifty further `self.env().get(&var_name)` reads take the container again for its
   metadata (declared type, shape, `is default`, immutability, the COW pointer before the
   mutation).
4. After the mutation it writes the container back by name (`set_env_with_main_alias`) and
   into the slot (`update_local_if_exists`, `write_local_slot_or_name`), and re-anchors any
   `HashEntryRef` local whose root was the pre-COW node.

The baked `target_slot` is consulted for the container only under `shadow_slots_active()`
(off by default) and only when no alias redirected the store. #14645 and #14650 made it
authoritative for the **constraint** (`StoreDecl`, ADR-13757), not for the container.

### 1.2 The slot is already the home everywhere else

For a local the code declares, every *read* already goes to the slot:

- a scalar reads `GetLocal(slot)`;
- a plain user `@`/`%` lexical reads `GetLocal(slot)` since ADR-0039 slice 2
  (`Compiler::container_read_slot`).

So the store keeps two copies of one binding and reconciles them around itself. That
reconciliation is a set of scaffolds whose only job is to make the env answer what the slot
holds:

- `seed_env_from_scalar_slot`: a scalar-held container skips its env mirror, so the handler
  copies the slot into the env before it runs, or it would mutate the declaration-time seed;
- `with_own_container_seeded`: a routine's own `my %seen` with no env binding in the frame
  was answered by a same-named file-scope variable of its compunit, so the slot is inserted
  into the env for the duration of the store and moved back afterwards;
- `with_package_lexical_seeded`, `attr_elem_env_snapshot` / `mirror_attr_elem_env_to_cell`:
  the same for a package-block lexical and an attribute's element;
- the post-store write-back into the slot, by name, which picks the *first* same-named slot
  unless the shadow build is on.

Each scaffold was added after a wrong answer in which the name resolved to a variable other
than the one the code declares. They are band-aids for one structural fact: the store asks
the name where the variable is, and only the slot knows.

### 1.3 Renaming the opcode retires nothing

#14632's done condition asks that `IndexAssignExprNamed` is no longer emitted for a
slot-resolved local. An opcode that carries the slot and calls the same body changes nothing:
the body would still resolve the container by name. The body itself has to take its container
from the slot. The previous slice's comment on #14632 reached the same conclusion and stopped
there.

### 1.4 ADR-14633 makes the env-centric body untenable

ADR-14633 (Proposed) decides that a call frame materializes no name-keyed `Env` until a write
by name asks for one. Under it, a routine's own `my @a` has no env entry at all. The
env-centric store would then need `with_own_container_seeded` on every element store of every
routine, and each seed is a by-name write that materializes the frame. The two ADRs are
compatible only if an element store into a declared local does not use the env.

## 2. Decision

**An element store resolves its target once, at the start of the store, to a place. For a
local the running code declares, the place is the slot: the store takes the container from
`self.locals[slot]` (through its binding cell, if it holds one), mutates it there, and never
looks the variable up by name. Only a target the code does not declare stores by name.**

### 2.1 The place

```rust
enum StorePlace {
    /// A local the running code declares (`BindingDesc::declared_here`).
    Slot { slot: usize, name: Symbol },
    /// Everything else: a free variable, an aliased target, an `our` or package
    /// variable, an `EVAL`'d or symbolic name, a dynamic, an attribute.
    Named { name: Symbol },
}
```

`name` on `Slot` is for diagnostics and the env mirror (§2.3), never for lookup.

**Which stores get `Slot`.** The compiler decides, from facts it has: the target is in
`local_map`, the slot is `declared_here`, and its reads already compile to `GetLocal` (a
scalar, or a plain user lexical `@`/`%` for which `container_read_slot` answers). The rule
is "**a store's place is where the same variable's reads look**": a variable whose reads stay
by name (`%_`, `@*dyn`, `%!attr`, a `::`-qualified name, an `our`) keeps a by-name store,
because its by-name read cascade supplies behaviours the slot does not (ADR-0039 §12).

**The two run-time conditions.** Two facts are not known at compile time. Both are checked
with a probe that is already latched per process, and both demote the place to `Named`:

1. **A sigilless alias record** exists for the name (`MetaNs::SigillessAlias`, only possible
   once `sigilless_alias_seen()`): the store goes to the variable the alias names, as now.
2. **A name-keyed cross-thread lane** answers the slot's read (the `__mutsu_atomic_*` and
   `shared_vars` probes that `exec_get_local_op_inner` makes before the slot): the store goes
   where that read would go. The rule is the read's, reused: a `Slot` place is valid exactly
   when a `GetLocal` of the slot would return the slot's own value.

Neither condition is a static analysis that can miss a case; a missed alias or lane keeps
today's by-name behaviour.

### 2.2 One body

The store body is not duplicated. Its container semantics (immutable `Map`/`List`, `Buf`,
native arrays, QuantHashes, Junction subscripts, slices, `:=` element binds, `is default`,
shapes) are a function of the container and the subscript, not of how the container was
found. The body is parameterized by the place, and three operations become methods of it:

- `place_root_mut(place)`: the container to mutate. `Slot` returns `&mut self.locals[slot]`
  descended through a `ContainerRef`; `Named` is today's `env_root_descended_mut_tracked`.
- `place_load(place)`: the container's current value, for the metadata reads of §1.1 step 3.
- `place_store(place, value)`: the write-back after a rebuild. `Slot` writes the slot (and
  the mirror, §2.3); `Named` writes by name as today.

The seeding scaffolds of §1.2 have no job for a `Slot` place and are not called for it. They
stay for `Named` until the stores that need them are slot-addressed or retired. The
`HashEntryRef` re-anchor after a COW stays for both: it is about the container node, not the
name.

### 2.3 The env mirror is written, never read

While a declared `@`/`%` local still has an env mirror (ADR-0039 §4.2's third bullet: every
container `SetLocal` writes it), a `Slot` store that replaces the container writes the mirror
afterwards, so the by-name readers that remain (`EVAL`, `MY::`, a by-name method call
writeback) see the same container. The store never reads the mirror. The write goes when the
mirror goes (ADR-0039 §4.2, ADR-14633). An in-place mutation of the slot's node needs no
mirror write, because the mirror holds the same node.

### 2.4 Divergence is a bug in a writer

The flip is only correct if the slot is current whenever a store runs. ADR-0097 §15 made
that an invariant for cells; this ADR extends it to every declared local an element store
can reach: **a site that replaces a declared local's container by name also replaces it in the
slot.** It is verified, not assumed:

- Before any store reads the slot, a check mode runs both resolutions for every `Slot` place
  and compares the containers (`Gc::ptr_eq` after descending cells; equality of value for an
  unboxed scalar holding no container). A mismatch names the writer that updated only one
  copy; that writer is fixed, never the check relaxed.
- The check is opt-in (an environment variable) until its inventory over `t/`, the whitelisted
  roast and the battery suite is empty, then on in every debug build.

### 2.5 The opcode

`IndexAssignExprNamed` keeps its by-name meaning for the targets that are not declared locals
and loses `target_slot`. A new `IndexAssignLocal { slot, name_idx, .. }` (same flags) is
emitted for a `Slot` place; its `slot` is not optional and is not gated on
`shadow_slots_active()`. A slot index that is out of range for the running frame is an
invariant violation (a debug assertion), not a fallback to the name. The same split applies to
the nested and deep-nested element stores (`IndexAssignExprNested`, `IndexAssignDeepNested`),
whose `target_slot` #14645 added, and to `IndexElemAutoviv`.

### 2.6 Whole-container stores take the slot's constraint too

The `SetLocal` / `AssignExprLocal` whole-container helpers (`coerce_typed_container_assignment`,
`coerce_hash_var_value`, `array_container_writethrough_value`,
`hash_container_writethrough_value`) take the store's `StoreDecl`, as the element stores do
since #14645. A store that holds a declared slot reads the constraint from it; a by-name store
(`SetGlobal`, the pseudo-stash) passes `None` and asks by name.

## 3. Consequences

- **Correctness.** A declared local's element store can no longer reach a same-named variable
  of a caller, a compunit or a package. The class of bug the seeding scaffolds patched one
  shape at a time stops existing for those stores.
- **Deletions.** For `Slot` stores: the alias-free env probe chain, the seven-way root
  precedence, the scaffolds of §1.2 and the by-name slot write-back. `target_slot`'s
  shadow-build gate goes from these ops.
- **Performance.** A store into a declared local does one slot access where it now does a
  hash probe per resolution step plus the write-back probes. Not the motivation; measured per
  slice under `perf-tuning` when a slice claims it.
- **ADR-14633** becomes implementable for element stores: a lazy frame's own locals are never
  looked up by name by them.
- **Cost.** Every helper the body calls with `var_name` must say which of its uses are a
  lookup (moved to the place) and which are diagnostics or sigil tests (kept). That is the bulk
  of the work, and it is mechanical once the place exists.

## 4. Slices (an order, not a progress log)

1. **The seam.** Introduce `StorePlace` and the three place methods; resolve the place once at
   the top of the store; route the body's container accesses through it, with `Slot` still
   resolving by name. No behaviour change. Add §2.4's check mode.
2. **The SetLocal helpers** (§2.6). No behaviour change for a declared slot (its record is in
   the running frame, ADR-13757).
3. **The inventory.** Run §2.4's check over `t/`, the whitelisted roast and the battery suite;
   fix each writer it names.
4. **The flip.** `Slot` resolves to the slot; the scaffolds are skipped for it; the check
   becomes a debug assertion.
5. **The opcode split** (§2.5), then the nested, deep-nested and autoviv stores.
6. **The push family on a local** (`CallMethodMut` with a declared receiver slot) takes the same
   place, so a method mutation of a declared local is slot-addressed too.

Each slice passes the gate with focus on `t/vm/binding/`, `t/vm/writeback/`, `t/types/`, the
whitelisted `roast/S02-types`, `S04-declarations`, `S09-typed-arrays`, and the bundled URI and
HTTP::UserAgent batteries (they caught ADR-12529 slice 45's regressions).

## 5. Alternatives rejected

- **A slot-carrying opcode that calls the existing body.** §1.3: renames, retires nothing.
- **Thread the slot through the name-keyed helpers and look it up with `find_local_slot`.**
  Rejected by the maintainer on #14139: a signature-level refactor that keeps the body
  env-centric and adds a by-name slot search per store, which is wrong under a shadow and costs
  more than the probe it replaces.
- **A small slot-native handler for the common shapes, falling back to the named body.** Two
  implementations of one primitive (AGENTS.md, ADR-0117), and the fallback is a new slow path.
- **Keep the env authoritative and seed it from the slot on every store.** Generalizes the
  scaffolds instead of deleting them, and contradicts ADR-0039 slice 2 (reads use the slot) and
  ADR-14633 (frames have no env).
- **Decide `Slot` vs `Named` by a whole-program analysis of aliases and lanes.** The ADR-0110
  hazard: correct only while the analysis is complete. §2.1's run-time checks are latched per
  process and cannot be wrong, only slower.

## 6. Open questions

- **`roast/S02-types/array.t` #62.** The shadow-slot gate exists because a baked slot was out
  of range for the running frame on a chained slice-lvalue store
  (`(@b[*-3,…] = @a) = …`). Under §2.5 that is an invariant violation; slice 4 has to find out
  which frame the store runs in there before the gate can go.
- **`state` and celled locals.** A `state @a` and a captured, cell-boxed local hold a
  `ContainerRef` in the slot; descending it is the same operation as on the env side. Slice 3's
  check confirms it rather than this ADR assuming it.
- **Thread clones.** A thread clone reads a shared `@`/`%` from `shared_vars` before its slot
  (`exec_get_local_op_inner`). §2.1's second condition demotes those stores to `Named`; whether
  a thread clone's store should instead write through the shared node is ADR-0068's question,
  not this one's.
