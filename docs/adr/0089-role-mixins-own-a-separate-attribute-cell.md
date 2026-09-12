# ADR-0089: A role mixin owns an attribute cell separate from its wrapped value

- **Status**: Proposed (revised after design review on 2026-09-12)
- **Date**: 2026-09-12
- **Related**: [ADR-0013](0013-container-interior-mutability-cellvalue.md) (one
  authoritative interior-mutable cell),
  [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md)
  (compiled attribute access and unified method dispatch), and
  [ADR-0060](0060-mixin-what-is-a-composition-keyed-type-object.md) (mixin
  composition identity)
- **Addresses**: [GitHub issue #8026](https://github.com/tokuhirom/mutsu/issues/8026)

## 1. Context

`ValueRepr::Mixin(Arc<Value>, Gc<MixinOverrides>)` currently has two kinds of
state in the same `MixinOverrides` map:

- method/type overrides and composition markers; and
- `__mutsu_attr__<name>` values, which are only construction-time seeds for
  attributes declared by a mixed-in role.

The seed map is not an attribute cell. Compiled `$!attr`, `$.attr`, `@!attr`,
and `%!attr` access uses the live `InstanceAttrs` cell when one exists. A mixin
around a native value has no such cell, so the compiled method runner falls
back to its entry snapshot. Its updates are then discarded. A mixin around an
`Instance` has the opposite failure: the role's attributes are committed to
the wrapped instance's cell, where the role did not declare them.

The bug is observable in both scalar and aggregate attributes:

```raku
role Counter {
    has $!n = 0;
    method bump() { $!n = $!n + 1 }
    method n() { $!n }
}
my %h does Counter;
%h.bump; %h.bump;
say %h.n; # 2
```

On current mutsu the method sees the seed on every call. The same loss occurs
for `Array`, `Int`/`Str`, and an `Instance` mixed with the role. The existing
`InstanceAttrs` cell already has the required sharing, interior mutability,
and GC tracing semantics; the missing decision is which cell owns the role's
state and how dispatch selects it.

## 2. Decision

### 2.1 State shape

Turn `MixinOverrides` from a type alias into a small state object containing:

```text
MixinOverrides {
    overrides: HashMap<String, Value>,
    attributes: Gc<InstanceAttrs>,
}
```

The `overrides` field keeps the existing marker and method-override protocol.
The `attributes` field is the authoritative store for attributes declared by
the role composition. `ValueRepr::Mixin` and the NaN-box `MixinBox` keep their
current two-payload shape: the new cell is owned by the already-existing GC
node rather than added as a third value payload.

`__mutsu_attr__<name>` entries remain seeds during the migration. Composition
copies a seed into the role cell only when that role attribute has no live
value yet. Reads and writes after construction never use the marker as the
store of record.

Cloning has two distinct meanings and must preserve them:

- cloning a `Value::Mixin` aliases its existing `Gc<MixinOverrides>` and
  therefore sees the same role-attribute writes, just as aliases of an
  `Instance` share its `Gc<InstanceAttrs>`;
- composing a new `but` mixin clones the `MixinOverrides` state, including a
  deep copy of `InstanceAttrs`, so the new value does not mutate the old
  value's role attributes.

The state object must trace the role cell in addition to tracing values in the
override map. Its deep clone must detach promoted `ContainerRef` values in the
same way as `InstanceAttrs::clone`. The role cell is storage only: it is never
wrapped in a `Value::Instance`, never used as a Raku object identity, and is
constructed with `queue_destroy = false`. Its `class_name`, `id`, `WHICH`
memo, and `DESTROY` fields are implementation details of the reused storage
type, not observable properties of the mixin.

Serialization MUST encode the current role-attribute cell separately from the
composition/override map. On decode, the serialized live cell is authoritative;
the `__mutsu_attr__*` entries are fallback seeds only for older serialized
values that have no live-cell payload. Aggregate values and promoted
`ContainerRef` values use the same recursive value serialization rules as
ordinary instance attributes.

The state object must not implement `Deref<Target = HashMap<...>>`. Map-only
accessors (`override`, `overrides`, and their mutation counterparts) must be
named separately from role-cell accessors so a caller cannot accidentally treat
the construction seed map as live attribute storage.

### 2.2 Two-store attribute resolution

For a method whose invocant is a role mixin, attribute resolution is ordered by
the method owner. The implementation exposes one selector contract for every
path that reads, writes, promotes, or reconciles an attribute:

```text
select_attr_store(MethodOwner, invocant, AttrKey)
    -> RoleCell | InnerCell | ClassLevel
```

`MethodOwner` carries the resolved owner kind and stable role declaration
identity. It must not be reconstructed from the display name
`Base+{Role,...}`. For a role owner, the selector uses the role declaration id
already recorded by `__mutsu_role_id__<name>` together with the role's
attribute key; if the language later permits two live applications of one
declaration that need independent state, the composition/application id is
also part of that key. A role's short display name is never the sole storage
identity.

The selector applies these rules:

1. if the current method belongs to a role composed on the invocant, consult
   the mixin's role-attribute cell;
2. if the requested attribute is absent there, consult the wrapped value's
   ordinary `InstanceAttrs` cell, if it has one; and
3. retain the existing class-level attribute fallback for public attributes.

For a method owned by the wrapped class, the order is reversed: the wrapped
instance cell is authoritative, and the role cell is not visible merely
because the invocant is wrapped in a mixin. This keeps a role's `$!n` separate
from a class's `$!n` even when both use the same short name.

The compiled read, scalar write, post-increment, container assignment,
accessor lvalue, and `:=` reconciliation paths must use this owner-aware
selection. A role method call must not pass a merged snapshot back to the
wrapped instance wholesale. Each store receives only the keys it owns. This
also applies to `nextsame`/`callsame`: a base-class method reached from a role
method writes the wrapped class cell, not the role cell.

Role attributes with the same name in different role declarations use a stable
owner-qualified key based on that identity. Public role accessors resolve
through the role composition and read the role cell, so they cannot resurrect a
stale `__mutsu_attr__` seed.

The selector and its commit protocol are shared by ordinary role dispatch,
qualified role dispatch, generic method fallback, accessor lvalues, and
`nextsame`/`callsame`. A method exit produces per-store deltas; it never commits
one merged map to whichever cell happens to be found by unwrapping the
invocant.

### 2.3 Construction and mutation

Every mixin composition creates or copies the role cell before running the
role's `BUILD`/`TWEAK` path. The role body and role methods use the same cell.
`does` on a real `Instance` remains the existing in-place rebless path: its
role attributes stay in the instance cell because that object has one shared
object identity. The separate mixin cell is required for `but` and for
`does`/`but` applied to non-`Instance` values.

The following existing paths must be migrated together with the core dispatch
change rather than retaining seed-specific exceptions:

- `dispatch_mixin_method_call` and its public role-attribute accessor;
- `self_instance_attrs` and the compiled attribute read/write helpers;
- `run_resolved_method_compiled_or_treewalk`'s final reconciliation;
- `methods_mut_method_lvalue` and computed/container attribute assignment;
- mixin `clone`, multidimensional delegated attribute mutation, and
  `BUILD`/`TWEAK` initialization;
- qualified dispatch in `methods_qualified.rs` and generic fallback in
  `methods_call_dispatch.rs`; and
- serialization/deserialization and the `Trace`/GC-edge-severing path for the
  new role cell.

No new VM-side interpreter or method-call fallback is introduced. The change
stays in the parser/compiler/VM attribute-cell and dispatch machinery already
used by ordinary instances.

## 3. Rejected alternatives

### 3.1 Keep the seed map as the live store

Rejected because it is a snapshot and has no shared-cell identity. Updating it
functionally at method return reproduces the lost-write bug whenever the
dispatch target and the variable hold different map nodes.

### 3.2 Commit role attributes into the wrapped instance cell

Rejected because `C.new but R` has two owners. The role's `$!n` must not become
an undeclared attribute of `C`, and a later `C` method must not read or overwrite
the role's state.

### 3.3 Add a third field to `ValueRepr::Mixin`

Rejected because it duplicates the existing GC node boundary and expands the
NaN-box `MixinBox` payload and every representation seam for no semantic gain.
Keeping the cell inside `MixinOverrides` preserves the current value shape and
its existing aliasing/tracing contract.

### 3.4 Store the cell as a hidden entry in the override map

Rejected because a hidden `Value::Instance` entry would leak into map equality,
serialization, composition-key extraction, and every map iteration unless all
consumers grew a second filtering convention. A typed state object makes the
ownership boundary explicit and keeps composition markers distinct from
attribute storage.

### 3.5 Expose the new state object as a `HashMap` via `Deref`

Rejected because it would make the old map-only call sites compile while
concealing the new ownership boundary. Those sites would continue to read
seeds, compare live state as composition metadata, or copy the map without
copying the role cell. Named map and cell APIs make each migration site
explicit.

### 3.6 Treat the role cell as a second Raku object

Rejected because reusing `InstanceAttrs` does not make the storage a user
visible instance. Giving it an object id, `.WHICH`, or `DESTROY` lifecycle would
create a third identity inside one mixin and would make GC finalization
observable. The cell is a private storage node only; if the implementation
cannot maintain that invariant with `InstanceAttrs`, it must first extract a
smaller GC-traceable attribute-store type rather than expose the cell as an
instance.

### 3.7 Copy the wrapped instance as part of this fix

Rejected as an unbounded scope expansion. This ADR defines that a new `but`
composition receives an independent **role** cell, while the wrapped value's
existing copy policy is unchanged. The known distinction between a Raku
`.clone`, a value-level alias, `but`, and in-place `does` for the wrapped
`Instance` is recorded and tested as a separate behavior question; it is not
silently changed by the role-cell implementation.

## 4. Acceptance criteria

The implementation slice is complete when all of the following are pinned by
focused tests and the existing full suites remain green:

1. The four forms in #8026 (`Hash`, `Array`, native scalar, and `Instance`
   mixin) print `bump -> 1`, `bump -> 2`, and `n == 2`.
2. A `C` attribute and a same-named role attribute remain independent through
   role and class methods, including `nextsame`/`callsame`.
3. Scalar, array, and hash role attributes retain writes across separate method
   calls and through public accessors; `Hash::Restricted`'s `allowed` set is
   visible to both `STORE` and `AT-KEY`.
4. Rust-level `Value` cloning aliases the mixin GC node; Raku `.clone` creates
   an independent role cell; aliases of one mixin share the role cell; and a
   copied `but` composition gets an independent role cell. In-place `does` on
   a real `Instance` keeps its existing object-cell rule. The wrapped
   instance's separate `but` copy policy is explicitly pinned as out of scope.
5. Role attributes survive serialize/deserialize after scalar, array, hash,
   and promoted-`ContainerRef` mutation; legacy seed-only serialized values
   still decode through the compatibility fallback.
6. Normal, qualified, private, `nextsame`, `callsame`, `is rw`, `:=`,
   multidimensional, and delegated-container paths all select and commit the
   correct store.
7. Same-name class/role attributes, different role declarations with the same
   display name, and sequentially applied roles do not collide. The role cell
   does not affect `==`, `eqv`, `===`, `.WHAT`, `.^name`, or `.^set_name` beyond
   the existing documented mixin semantics.
8. A self-referential role attribute is traced and its GC edges are severed
   correctly under the existing GC tests.
9. Existing mixin type identity and `.WHAT`/`.^name` behavior from ADR-0060 is
   unchanged.

## 5. Implementation plan

The code PR should land these as one behaviorally complete slice, with a
focused regression test and a `news/` entry:

1. Introduce the `MixinOverrides` state object, named map/cell APIs, stable
   role-attribute keys, deep clone, live-cell serialization, and GC tracing /
   edge-severing tests.
2. Add the `MethodOwner`/`select_attr_store` contract and migrate compiled
   reads, writes, lvalue promotion, and method-exit reconciliation.
3. Route ordinary, qualified, and generic fallback dispatch through the same
   selector; migrate role accessors, delegated container mutation,
   `BUILD`/`TWEAK`, and Raku `.clone`; remove seed-as-store fallbacks.
4. Pin the `but`/`does`/`.clone` copy boundary and equality/type-identity
   invariants.
5. Add the #8026 and `Hash::Restricted` regressions, then run the full local
   test and roast gates.

The issue stays open while this Proposed ADR is reviewed. Once the design is
accepted, implementation progress belongs in this ADR's status and the
follow-up PR that closes #8026.
