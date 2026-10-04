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
   has already lost the distinction.

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

`bootarray`, `boothash`, `bootint`, `bootintarray`, `bootnum`,
`bootnumarray`, `bootstr` and `bootstrarray` answer type names `BOOTArray`,
`BOOTHash`, `BOOTInt`, `BOOTIntArray`, `BOOTNum`, `BOOTNumArray`, `BOOTStr`,
`BOOTStrArray` respectively. Their REPR names are `VMArray`, `VMHash`,
`P6int`, `VMArray`, `P6num`, `VMArray`, `P6str`, `VMArray` respectively.
`iscoderef(sub {})` is 0, while
`iscoderef(nqp::getattr(sub {}, Code, '$!do'))` is 1: a high-level callable
and its executable code body are different objects.

## 3. Proposed decision

### 3.1 Preserve the representation on the value, not in a side registry

Represent the raw VM identity as part of the value's own backing object.
Collection backings distinguish VMArray/VMHash from high-level Array/Hash;
the marker survives aliases, itemized holders, element access, cloning and
argument passing. BOOT integer, number and string boxes have their own value
shape instead of reusing the high-level `Int`, `Num` and `Str` variants. The
code body read from `Code.$!do` likewise has a distinct MVMCode shape, with
the existing body identity and call behavior retained.

The eight BOOT type objects have explicit identities and REPR metadata. The
corresponding constructors (`list`, `hash`, typed `list_*`, `box_*`, `create`)
produce values with that identity, and `WHAT`, `reprname`, `unbox_*`, storage
operations and the five type tests read the same representation. A type object
itself passes its REPR test, as Rakudo does. This is a BOOT-specific promise:
it does not claim that every existing high-level mutsu value now has a complete
MoarVM body (ADR-0015 §5).

The representation bit must live on the shared backing, not on a temporary
`Value` holder. Two aliases to the same VM collection must answer alike after
itemization or a parameter bind. Adding a name-keyed registry or inferring the
representation from the collection's elements would violate that invariant.

### 3.2 Preserve the lvalue used as an NQP operand

The compiler lowers the four container tests from an lvalue expression to a
typed reference descriptor. The descriptor identifies a lexical, attribute
or element storage location, its native kind (object, int, num or str), and
whether this *binding* is writable. A read-only parameter gets a read-only
descriptor even when the caller supplied a writable cell; an `is rw` parameter
keeps the writable descriptor. `my int $i` retains its native kind instead of
round-tripping through a generic `.VAR` `Scalar`.

The descriptor is transient call data, not an extra `Interpreter` field or a
second store for the value. Its location points to the existing slot/cell and
uses that storage's locking rules. Ordinary expression evaluation still reads
the value. The compiler handles non-lvalue expressions as bare values, so
`isrwcont(1)` and `iscont_i(1)` return 0 without constructing a fake container.
The existing `iscont` uses this same lowering and stops relying on `.VAR`
class names. This keeps the five container tests consistent across lexical,
parameter, attribute and element forms.

### 3.3 Keep one implementation per predicate

One representation classifier supplies `islist`, `ishash`, `isstr`, `isint`,
`isnum` and `iscoderef`; one descriptor classifier supplies `iscont`,
`iscont_i/n/s` and `isrwcont`. The `nqp::` op tables expose those classifiers
through `OpCode::NqpOp`, with the necessary lvalue lowering in the compiler.
No new tree-walk or interpreter method-dispatch fallback is introduced.

## 4. Rejected approaches

- **Treat every Raku value of a matching type as its VM REPR.** This gives
  `isstr("a")`, `ishash({})` and `islist([])` the opposite of Rakudo's result.
- **Always return zero, except for a few recognized literals.** The tests
  would then depend on source spelling rather than an object's representation.
  An alias or `box_*` call would immediately disagree with its source.
- **Return BOOT names as ordinary `Package` values.** `reprname`, `WHAT`,
  `create` and boxing would have no matching body or type identity.
- **Infer writable/native status from `.VAR`'s Raku class or from the
  contained value.** A read-only parameter and an `is rw` parameter can
  refer to the same caller variable but must answer differently.
- **Classify the operands in `runtime/methods.rs`.** This would add the
  forbidden tree-walk slow path and still discard lvalue information before
  the classification.

## 5. Delivery and acceptance

1. Pin the Rakudo matrix above in focused `t/vm/` tests, including BOOT type
   objects, raw-versus-Raku collections, aliases, `Code.$!do`, native
   lexicals, read-only and `is rw` parameters, and non-lvalue operands.
2. Add BOOT type and value identities and update all existing operations that
   construct, inspect, serialize or trace those value shapes. Include
   `nqp::islist` in the same representation pass.
3. Add the lvalue descriptor compiler/VM path, then implement all five
   container tests through it. Pin that writes still reach the original
   location and that a read-only parameter never gains write access.
4. Register all 17 ops in `NQP_OPS`, rerun
   `scripts/nqp-op-coverage.py`, and update `docs/nqp-op-coverage.md` from
   36/53 to 53/53 Type / Conversion ops. The tests must agree with Rakudo;
   mere absence of `Unsupported nqp:: op` does not satisfy acceptance.

This ADR proposes one mechanism and its acceptance criteria. No op is marked
implemented or "Not applicable" before the mechanism is delivered.
