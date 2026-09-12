# ADR-0088: A role mixin owns an attribute cell separate from its wrapped value

- **Status**: Proposed
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
same way as `InstanceAttrs::clone`. Serialization writes the existing visible
override map and reconstructs a fresh role cell from the attribute seeds.

### 2.2 Two-store attribute resolution

For a method whose invocant is a role mixin, attribute resolution is ordered by
the method owner:

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

Role attributes with the same name in different role declarations use the
existing owner-qualified attribute-key convention (extended with the role's
declaration identity where necessary). Public role accessors resolve through
the role composition and read the role cell, so they cannot resurrect a stale
`__mutsu_attr__` seed.

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
  `BUILD`/`TWEAK` initialization.

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
4. Aliases of one mixin share the role cell, while a copied `but` composition
   gets an independent cell. `.clone`, `does`, and `but` preserve this rule.
5. A self-referential role attribute is traced and reclaimed correctly under
   the existing GC tests.
6. Existing mixin type identity and `.WHAT`/`.^name` behavior from ADR-0060 is
   unchanged.

## 5. Implementation plan

The code PR should land these as one behaviorally complete slice, with a
focused regression test and a `news/` entry:

1. Introduce the `MixinOverrides` state object, constructors, deep clone,
   serialization, and GC tracing tests.
2. Add owner-aware role/inner cell selection and migrate compiled reads,
   writes, and method-exit reconciliation.
3. Migrate role accessors, lvalues, delegated container mutation,
   `BUILD`/`TWEAK`, and `clone`; remove seed-as-store fallbacks.
4. Add the #8026 and `Hash::Restricted` regressions, then run the full local
   test and roast gates.

The issue stays open while this Proposed ADR is reviewed. Once the design is
accepted, implementation progress belongs in this ADR's status and the
follow-up PR that closes #8026.
