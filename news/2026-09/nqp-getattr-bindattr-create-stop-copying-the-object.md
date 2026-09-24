# `nqp::getattr` / `bindattr` / `create` stop copying the object

Second slice of [ADR-0121](../../docs/adr/0121-instance-attributes-live-in-per-class-slots.md) D1
(#9291, #9134 group 1 and `create`).

## What was paid per call

- **`nqp::getattr`** cloned the object's whole attribute map (`to_map()`) to read one key.
  It also built a `Vec<String>` of candidate keys and copied the name operand into an owned
  `String`. On an instance, `is_match_instance` then hashed the cursor-marker string to probe
  for it.
- **`nqp::bindattr`** (and `p6bindattrinvres`) cloned the whole map, inserted one key, and
  committed the clone back over the original.
- **`nqp::create`** rebuilt the class's slot list on every call: an MRO walk collecting the
  attribute definitions, then a type-constraint resolution per attribute. It also scanned
  every registered `VMHash` / `VMArray` class, with an `rsplit` per entry, to match the
  class by short name.

## What changed

- getattr reads its one key under the attribute map's read guard. The name operand is
  borrowed, and the bare name and the twigil spelling are probed with no allocation.
- bindattr writes its one key in place under the write lock
  (`InstanceAttrs::bind_attr_through`). If this thread already holds a read guard on the
  cell, the write is queued the way `commit_attrs` queues a whole-map write.
- The per-class constructor plan (`NativeCtorPlan`) now carries `create_slots`, the
  `CREATE` template. It is built once per class and invalidated wherever the plan is;
  `nqp::create` and `.CREATE` copy it.
- The VM storage class sets are keyed by short name, so matching a class is one probe.
- The cursor marker `is_match_instance` probes for is interned once.

## Measured

Callgrind, per op inside a TRIR routine (`nqp::while` loop, object passed as a parameter),
1 vs 10,001 iterations above the empty loop:

| op | before | after |
| --- | ---: | ---: |
| `getattr($o, IB, '$!a')` | 8,087 Ir | 6,486 Ir |
| `bindattr($o, IB, '$!b', $i)` | 7,310 Ir | 6,475 Ir |
| `create(IB)` | 12,698 Ir | 7,969 Ir |

About 4,700 of what is left on each of these rows is not the op at all. It is the user
class operand `IB`, a bareword that is resolved again on every execution. That resolution
checks for an imported nullary routine of the same name, runs `has_type` twice, and
resolves the type object's name. It is a general bareword-resolution cost, not part of this
slice. The `$!reified` / `$!storage` bind still copies the storage's elements (#9134
group 1). D2/D3 remain for the attribute probe itself.

Pinned by `t/vm/nqp-attr-ops-slots.t`.
