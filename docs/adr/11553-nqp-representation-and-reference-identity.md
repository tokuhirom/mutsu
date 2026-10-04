# ADR-11553: Keep NQP representation and reference identity across value operations

- **Status**: Proposed (awaiting maintainer decision; no implementation has landed)
- **Date**: 2026-10-04
- **Issue**: [#11553](https://github.com/tokuhirom/mutsu/issues/11553)
- **Related**: [ADR-0005](0005-nanbox-representation-encoding.md) (value tags),
  [ADR-0015](0015-native-backed-container-storage-and-repr-bodies.md) (native
  storage and public REPR claims),
  [ADR-0064](0064-var-descriptor-carries-the-contained-value.md) (`.VAR`),
  [ADR-0097](0097-a-binding-descriptor-addressed-by-slot.md)
  (slot-addressed binding metadata), and
  [#10956](https://github.com/tokuhirom/mutsu/issues/10956) (collection
  element descriptors).

## 1. Problem

Seventeen reachable `nqp::` ops remain unsupported because their answers
depend on information mutsu currently discards. This is one representation
problem with three observable parts:

1. `nqp::list` and an ordinary Raku `Array` both become `Value::array`, and
   `nqp::hash` and an ordinary Raku `Hash` both become `Value::hash`. The
   existing `nqp::islist` therefore answers 1 for both, although Rakudo answers
   1 only for the raw VM list. The five missing REPR tests need the same
   distinction. An `Int`, `Num`, `Str`, `Hash` or `Sub` Raku object is not a
   BOOT box, VMHash or MVMCode merely because its Raku type sounds similar.
2. The eight `boot*` ops return real type objects. `box_i`, `box_n`, `box_s`,
   `create`, typed VM arrays, `WHAT` and `reprname` must agree about those
   objects; returning an existing Raku type object with a BOOT name would make
   the type tests lie.
3. `nqp::iscont_i/n/s` inspect a native lvalue reference, while
   `nqp::isrwcont` inspects whether the *binding passed to that call* is
   writable. A native lexical's `.VAR` is currently a generic `Scalar`, and a
   read-only parameter can refer to a caller's writable container. The
   existing `nqp::iscont` compiler form converts its argument to `.VAR`, which
   has already lost the distinction. A native element reference returned by
   `nqp::atposref_i/n/s` can also be bound to another name and inspected later;
   this is a first-class container, not only a property of a call's syntax.

The coverage campaign [#11488](https://github.com/tokuhirom/mutsu/issues/11488)
requires a real implementation or a justified "Not applicable" entry for each
op. These ops are applicable: a Raku program can call them and get meaningful
answers. A zero result for every operand, a type name without its representation,
or a heuristic based on the contained Raku value would silently misreport them.

## 2. Rakudo observations

Measured with `use nqp` on 2026-10-04:

| Probe | Raw / writable | Raku object / read-only |
| --- | --- | --- |
| `islist(nqp::list())` / `islist([])` | 1 | 0 |
| `ishash(nqp::hash())` / `ishash({})` | 1 | 0 |
| `isstr(box_s("a", bootstr()))` / `isstr("a")` | 1 | 0 |
| `isint(box_i(1, bootint()))` / `isint(1)` | 1 | 0 |
| `isnum(box_n(1e0, bootnum()))` / `isnum(1e0)` | 1 | 0 |
| `iscont_i(my int $i)` / `iscont_i(my $x)` | 1 | 0 |
| `isrwcont(my $x)` / `isrwcont(1)` | 1 | 0 |
| `isrwcont($p is rw)` / `isrwcont($p)` | 1 | 0 |
| `iscont_i(atposref_i(list_i(1), 0))` / `iscont_i(atpos_i(list_i(1), 0))` | 1 | 0 |

`bootarray`, `boothash`, `bootint`, `bootintarray`, `bootnum`,
`bootnumarray`, `bootstr` and `bootstrarray` answer type names `BOOTArray`,
`BOOTHash`, `BOOTInt`, `BOOTIntArray`, `BOOTNum`, `BOOTNumArray`, `BOOTStr`,
`BOOTStrArray` respectively. Their REPR names are `VMArray`, `VMHash`,
`P6int`, `VMArray`, `P6num`, `VMArray`, `P6str`, `VMArray` respectively.
`iscoderef(sub {})` is 0, while
`iscoderef(nqp::getattr(sub {}, Code, '$!do'))` is 1: a high-level callable
and its executable code body are different objects.
`hllize(nqp::list(1, 2))` and `hllize(nqp::hash("x", 1))` return high-level
`List` and `Hash` objects with `P6opaque` representation. They are not the
same objects as their raw inputs (`eqaddr` is 0), so `islist`/`ishash` change
from 1 to 0. Yet a raw `bindpos`/`bindkey` write is visible through the
high-level result: the two objects share element storage. A raw native element
reference remains a native container after `my $r := atposref_i(...)`:
`iscont_i($r)` and `isrwcont($r)` are both 1. Its `.VAR` is not a substitute
for the reference operand (`iscont_i($r.VAR)` is 0).
`hllize(box_i(42, bootint()))` and `hllize(box_s("x", bootstr()))`
likewise produce high-level `Int` and `Str` values for which `isint` and
`isstr` change from 1 to 0.

## 3. Proposed decision

### 3.1 Separate object identity from element storage

Represent the raw VM identity on the *object*, separately from its element
storage. A raw VMArray/VMHash and a high-level `List`/`Hash` created by
`hllize` must be different objects with different REPR answers while sharing
mutable storage. The raw object's type/REPR identity survives aliases,
itemized holders and argument passing; `hllize` creates a high-level object
that points to the same storage. An explicit clone follows Rakudo's clone
semantics rather than blindly copying an origin flag.

This requires an object shell separate from the storage node for raw
collections and their high-level wrappers. `ArrayData`/`HashData` currently
combine contents and `.WHICH` identity, so a field added to either one cannot
by itself satisfy the `hllize` observation: the raw and high-level objects
would have the same REPR or the same identity. The implementation must
separate those concerns while retaining the shared mutation and GC tracing
rules. A new tag or wrapper is an implementation choice to be checked against
ADR-0005's eight-byte `Value` and the JIT's tag assumptions.

BOOT integer, number and string boxes likewise need raw object identity
distinct from high-level `Int`, `Num` and `Str`. The executable code body
read from `Code.$!do` needs MVMCode identity distinct from the high-level
routine. `code_do_attr.rs` already constructs a distinct direct-code `Sub`
and records its identity with `__mutsu_wrap_direct`. The implementation should
move that fact to an explicit code-body kind used by both existing direct-call
dispatch and the REPR classifier, rather than create a second, unrelated
code-body mechanism or add another name-keyed marker.

The eight BOOT type objects have explicit identities and REPR metadata. The
corresponding constructors (`list`, `hash`, typed `list_*`, `box_*`, `create`)
produce values with that identity, and `WHAT`, `reprname`, `unbox_*`, storage
operations and the five type tests read the same representation. A type object
itself passes its REPR test, as Rakudo does. This is a BOOT-specific promise:
it does not claim that every existing high-level mutsu value now has a complete
MoarVM body (ADR-0015 §5).

Two aliases to the *same object* must answer alike. A raw object and its HLL
wrapper must answer differently even when they share storage. A name-keyed
registry or a classification inferred from elements would violate both rules.

### 3.2 Preserve both the binding view and first-class references

The compiler preserves the lvalue of an NQP container-test operand instead
of compiling it as an ordinary decontainerized value. For a lexical,
attribute or element it carries the storage location, native kind (object,
int, num or str), and whether this *binding* is writable. A read-only
parameter gets a read-only binding view even when the caller supplied a
writable cell; an `is rw` parameter keeps a writable view. `my int $i`
retains its native kind instead of round-tripping through a generic `.VAR`
`Scalar`. Literal and other bare-value operands have no container.

This call-site view is insufficient for `atposref_*` and other first-class
references: their native kind, writable status and storage identity must live
on the reference value and survive `:=`, captures and argument passing. A
native lexical reference that escapes a frame must use a managed cell, not a
raw pointer into the frame's local array. Reuse the existing `VarRef`,
`CaptureVarCell`, native positional-reference and slot-addressed binding
metadata mechanisms where they can represent this information. Extend one
of them only after checking its alias and lifetime behavior; do not add a
parallel name-keyed registry or a new `Interpreter` field.

`iscont_i/n/s` and `isrwcont` classify both the direct binding view and an
already materialized reference. The existing `iscont` should use the same
reference facts where applicable; `.VAR` class-name inference alone cannot
answer the native-kind or writability tests. The compiler and VM must agree
for the ordinary `NqpOp` path, TRIR and any JIT inlining.

### 3.3 Keep one implementation per predicate

One object-REPR classifier supplies `islist`, `ishash`, `isstr`, `isint`,
`isnum` and `iscoderef`; one reference classifier supplies `iscont`,
`iscont_i/n/s` and `isrwcont`. The `nqp::` op tables expose those classifiers
through bytecode, with the necessary lvalue lowering in the compiler. No new
tree-walk or interpreter method-dispatch fallback is introduced.

## 4. Rejected approaches

- **Treat every Raku value of a matching type as its VM REPR.** This gives
  `isstr("a")`, `ishash({})` and `islist([])` the opposite of Rakudo's result.
- **Always return zero, except for a few recognized literals.** The tests
  would then depend on source spelling rather than an object's representation.
  An alias or `box_*` call would immediately disagree with its source.
- **Return BOOT names as ordinary `Package` values.** `reprname`, `WHAT`,
  `create` and boxing would have no matching body or type identity.
- **Put one raw/HLL flag on the shared `ArrayData`/`HashData`.** `hllize`
  returns a different object that shares the raw object's storage; one flag
  on that storage cannot give them different REPR answers.
- **Infer writable/native status from `.VAR`'s Raku class or from the
  contained value.** A read-only parameter and an `is rw` parameter can
  refer to the same caller variable but must answer differently; a native
  positional reference can be assigned to a new name and tested later.
- **Only carry an lvalue descriptor for the duration of one call.** This
  misses a first-class `atposref_*` result bound with `:=` and tested later.
- **Classify the operands in `runtime/methods.rs`.** This would add the
  forbidden tree-walk slow path and still discard lvalue information before
  the classification.

## 5. Delivery and acceptance

1. Pin the Rakudo matrix above in focused `t/vm/` tests, including BOOT type
   objects, raw-versus-Raku collections, aliases, `Code.$!do`, native
   lexicals, read-only and `is rw` parameters, non-lvalue operands,
   `hllize`'s distinct object/shared mutation, and a native element
   reference retained through `:=` and a call. Cover scalar `hllize` as well.
2. Separate raw and HLL object identities from their shared collection
   storage; add BOOT type and value identities and update all operations that
   construct, inspect, serialize or trace those shapes. Include `hllize`,
   `hlllist`, `hllhash` and `islist` in the same representation pass. Pin the
   eight-byte `Value`, collection sizes, GC and JIT invariants.
3. Extend the existing reference/call-argument machinery to preserve both
   direct binding views and first-class native references, then implement
   all five container tests through it. Pin that writes still reach the
   original location and that a read-only parameter never gains write access.
4. Register all 17 ops in `NQP_OPS`, rerun
   `scripts/nqp-op-coverage.py`, and update `docs/nqp-op-coverage.md` from
   36/53 to 53/53 Type / Conversion ops. The tests must agree with Rakudo;
   mere absence of `Unsupported nqp:: op` does not satisfy acceptance.

This ADR proposes one mechanism and its acceptance criteria. No op is marked
implemented or "Not applicable" before the mechanism is delivered.
