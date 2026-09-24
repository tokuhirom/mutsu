# ADR-0121: Instance attributes live in per-class slots, and each access site resolves its slot once

- **Status**: Proposed (2026-09-24)
- **Deciders**: tokuhirom (pending), Claude
- **Context**: [#9291](https://github.com/tokuhirom/mutsu/issues/9291) (the measurements),
  [#9134](https://github.com/tokuhirom/mutsu/issues/9134) group 1 and `create` (the `nqp::`
  per-call deficits), [#8673](https://github.com/tokuhirom/mutsu/issues/8673) (JSON::Fast, where
  these ops are ~5% of the decode). Touches [ADR-0013](0013-container-interior-mutability-cellvalue.md)
  (container interior mutability), [ADR-0068](0068-cross-thread-container-writes-need-a-synchronized-store.md) (cross-thread
  container writes), [ADR-0089](0089-role-mixins-own-a-separate-attribute-cell.md) (a mixin's role cell),
  [ADR-0115](0115-core-type-names-in-nqp-operands-fold-at-parse-time.md) (CORE type names are
  constants in `nqp::` operands).

## 1. Context

### 1.1 The numbers

Measured on `main` on 2026-09-24, with a release+debuginfo build on a 4-core container, against
rakudo 2026.07. Each figure is in-process and steady state.

Plain Raku classes, in ns per iteration of a `while` loop:

| | mutsu | rakudo |
|---|---:|---:|
| empty loop in a method (baseline) | ~200 | ~20 |
| read `$!x` | ~690 (+~450) | ~20 (+~0) |
| write `$!y = $i` | ~1,450 (+~1,250) | ~58 (+~35) |
| accessor `.x` (mainline loop) | ~1,300 | ~58 |
| `P.new(x => .., y => ..)` | ~4,600 | ~1,000 |

`nqp::` object-model ops, in ns per op inside a TRIR routine, where the loop costs almost nothing:

| op | mutsu | rakudo |
|---|---:|---:|
| `getattr(@r, Array, '$!descriptor')` | 220–280 | 13 |
| `bindattr(@r, List, '$!reified', $b)` | 460–530 | 15 |
| `getattr(%h, Map, '$!storage')` | ~205 | 12 |
| `create(IterationBuffer)` | ~530 | ~75 |
| `my @a` / `my %h` | 80–105 | ~6 |

Every attribute access is **20–30x** rakudo. That affects every class-based program, not only
`nqp::`-heavy modules. The empty-loop row is a different problem: method bodies do not enter
TRIR. It is out of scope here and noted in §7.

### 1.2 Where a `$!x` read goes

Callgrind, 1 vs 10,010 iterations of a method loop that reads a private `$!x`, differenced
against the same loop without the read. The read costs **~3,400 instructions**, spread thinly:

- `read_attr_cell_by_key` (`src/vm/vm_var_assign_computed_attr.rs`) does all of this on **every**
  read:
  - `get_env_self()` resolves `self` **by name** through the env;
  - `method_class_stack_top_str()` finds the owner class;
  - `is_role(owner)` checks the registry by string;
  - `method_attr_cells` walks up to 8 wrapper levels;
  - `attr_key_in_map` builds the qualified private key twice (`format!` + `Symbol::intern`, which
    explains the `malloc` and `Symbol::intern` in the profile), probes the hash with it, then
    probes with the bare key.
- `as_map()` takes the instance's `RwLock` and pushes and pops the cell address on the
  thread-local `HELD_READ_CELLS` (`src/value/value_instance.rs`).
- Storing the read value into `$s` takes the general `SetLocal` cascade, ~850 instructions.

`CompiledCode::local_attr_key` already moved the twigil parsing to once per chunk. Everything
listed above is still per read.

### 1.3 How attributes are stored

- **Instances.** An instance is `ValueRepr::Instance { class_name, attributes: Gc<InstanceAttrs>, id }`.
  The attributes live in `InstanceAttrs.attributes: Arc<RwLock<AttrMap>>`, and `AttrMap` is an
  `FxHashMap<Symbol, Value>`.
- **Keys.**
  - A public or unique name is stored bare (`x`).
  - A name used with two sigils in one class gets a sigil-prefixed key (`attribute_storage_key`).
  - A private attribute declared in both a parent and a child gets a qualified key `Owner\0bare`.
- **No layout.** There is no per-class layout. The key is re-derived and hashed on every access.
- **Construction.**
  - `.new` and `bless` build from a per-class cached `NativeCtorPlan` (`src/runtime/mod.rs`).
    It already knows the attribute list in order (`class_attrs`, `attr_syms`, `attr_index`,
    `attr_seeds`) and is invalidated at the registry mutation sites.
  - `nqp::create` does not use it. On every call it runs `collect_class_attributes` (an MRO walk
    with a registry lock per level, cloning every `ClassAttributeDef`) plus
    `get_attr_type_constraint` per attribute.
- **`nqp::getattr` / `bindattr`.**
  - Both ignore the class operand, `to_string_value` the name, and `to_map()` the whole
    attribute map on each call. `bindattr` then writes it all back with `commit_attrs`.
  - On a plain `my @a` / `my %h`, which are `ArrayData` / `HashData` rather than instances:
    - only `$!reified` / `$!storage` are answered;
    - `getattr(@a, Array, '$!descriptor')` is `Nil`, and a Hash answers itself for any name;
    - `bindattr` of anything else is a silent no-op.
- **`nqp::attrinited`** is not implemented.

In MoarVM a `P6opaque` object stores its attributes at offsets computed when the class is
composed. `getattr` with a known class and name is a load at a fixed offset, and spesh resolves
that offset once per site.

## 2. Decision

Four parts, landed in this order. Each part is useful on its own, and later parts depend on
earlier ones.

### D1. Stop re-deriving what an access site already knows (no representation change)

- **`$!x` / `$.x` inside a method.**
  - The chunk's attribute-key table (`local_attr_key`) carries the qualified private keys already
    built: `Owner\0bare` and its sigil form, interned once. No `format!` or `intern` per read.
  - `self` is read from the method frame's own slot, not by name through the env.
  - Whether the owner is a role is decided when the method is compiled and carried in the chunk,
    not looked up per read.
- **`nqp::getattr` / `bindattr` and their `_i` / `_n` / `_s` forms, and `p6bindattrinvres`.**
  - A constant name operand is interned once, at compile time. That covers the untyped
    `OpCode::NqpOp` and TRIR's `NqpOpGen` alike.
  - The read happens under `as_map()`, with no `to_map()`. The write inserts one key with no
    `commit_attrs` of the whole map.
  - The `$!reified` / `$!storage` bind shares the storage node instead of copying its elements
    (#9134 group 1).
- **`nqp::create` builds from the cached per-class plan**: the same seeds `.new` uses, cached per
  class and invalidated where `NativeCtorPlan` is. The linear `vmhash` / `vmarray` class scan is
  replaced by a by-name set lookup (#9134 group 3).

D1 removes the `malloc`, `intern` and registry lookups from every access. It leaves the hash
probe and the lock.

### D2. A per-class layout, and instances store declared attributes in slots

- **`ClassLayout`** is built once per composed class, from the data `NativeCtorPlan` already
  computes. It holds:
  - the declared attributes in MRO order, parents' first;
  - each attribute's storage key (the same keys as today: bare, sigil-prefixed, or
    `Owner\0bare`);
  - a key → slot index map;
  - a unique `layout_id`.

  It lives in a per-class cache with the same invalidation sites as `NativeCtorPlan`. A class
  whose layout changes after instances exist (`augment`, a later role composition) gets a new
  `layout_id`. Instances keep the old layout, which stays valid for them.
- **`AttrMap` becomes `AttrStore { layout: Arc<ClassLayout>, slots: Vec<Value>, extra: Option<Box<FxHashMap<Symbol, Value>>> }`**,
  behind the **same API** (`get`, `insert`, `contains_key`, iteration). Callers that use keys
  keep working unchanged:
  - A key is looked up through `layout.index` first and falls back to `extra`.
  - `extra` holds what is not declared. For example, a class with a builtin base
    (`attrs_fully_known == false`: `message`, `payload`, …) keeps attributes outside the
    registry.
  - An attribute that exists in the layout but was never set is an **absent** slot, a dedicated
    sentinel distinct from `Nil`. `contains_key` and the future `attrinited` answer from it.
- **Iteration order** (`.^attributes`, `.raku`, `eqv`) follows the layout, then `extra`. That is
  declaration order, which is what rakudo shows. The hash order it replaces was an accident.
- **A mixin (`does`) re-blesses in place.** It swaps to the mixin type's layout, which is the
  base layout plus the role's attributes appended, and grows `slots`. ADR-0089's separate role
  cell is left as it is. This ADR does not merge it.
- The lock is unchanged: `InstanceAttrs` still guards the store with its `RwLock`, and
  ADR-0068's write discipline still applies.

### D3. Each access site resolves its slot once, through an inline cache keyed by layout

- **`$!x` / `$.x` read and write sites**: a per-site cache `(layout_id → slot)`.
  - On a hit, the access is `slots[slot]` under the read lock.
  - On a miss, it resolves the key through the layout, as D1 does, and refills the cache.
  - A subclass instance shares its parent's prefix of slots. So a parent method's `$!x` site
    gets the same slot for every subclass under single inheritance, but the cache still keys on
    `layout_id` and does not rely on that.
- **Generated accessors** (`.x` with no user override): the call site's method cache already
  identifies the generated accessor, so it reads the slot directly and never enters method
  dispatch.
- **`nqp::getattr` / `bindattr` with a constant class and name** get the same per-site cache.
  ADR-0115 makes CORE class operands constants. A user class operand is a `LoadBareWord` that is
  resolved at run time, so its site caches on the receiver's `layout_id` instead.
- **`Array` / `Hash` pseudo-attributes.** `ArrayData` and `HashData` answer `$!reified`,
  `$!storage` and `$!descriptor` from a fixed table:
  - `$!descriptor` is synthesized from `value_type` / `declared_type` / `default`, which the
    containers already carry;
  - binding `$!descriptor` updates those fields;
  - `nqp::p6scalarwithvalue` reads the descriptor it is given.

  This is a correctness fix as well: `Nil` and silent no-ops become real answers.

### D4. `nqp::attrinited`

`attrinited` is implemented against D2's absent sentinel.

## 3. Consequences

- **Scope.** Every OO program pays less per attribute access, not only `nqp::` code. D2 and D3
  are the only way to reach 2x of rakudo (#9291's close condition). D1 alone leaves a hash probe
  plus a lock plus the `SetLocal` cascade per access.
- **Blast radius.** `AttrMap` is used widely. D2 keeps its key-based API, so the change is
  contained in `src/value/attr_map.rs` / `value_instance.rs` plus construction and rebless. What
  it changes observably is iteration order, and that change is toward rakudo's order.
- **Guards.** Each part is measured with #9291's two tables before and after, and with the
  callgrind 1-vs-N difference for `$!x` (§1.2). The bench CI's `bench-class` / `bench-ctor` rows
  are the numbers to quote.
- **GC.** A slot vector is traced like the map it replaces. `extra` is traced too. There are no
  new edges and no new interior-mutability primitive.

## 4. Rejected alternatives

- **Hidden classes / shape transitions (V8-style).** Raku classes are declared and composed
  before instances exist, so one static layout per class is enough. The rare dynamic key goes
  into `extra`. Shape transitions would add a mechanism for a case that almost never happens.
- **Stop at D1.** It removes the allocation and string work, but every access still pays a hash
  probe keyed by a Symbol that is compared against the map's own keys, a lock, and the
  thread-local guard bookkeeping. The measurement in §1.2 puts D1's share at well under half of
  the ~3,400 instructions.
- **Lock-free slot reads** (per-slot atomics or a seqlock). This may be worth doing after D3, but
  it interacts with ADR-0068's cross-thread discipline and needs its own ADR. Measure after D3
  first.
- **Remove the separate mixin role cell (ADR-0089) at the same time.** It is independent of slot
  layout and would double this change's blast radius.

## 5. Implementation status

Not started. D1 is the first slice.

## 6. Reproduction

The two tables in §1.1 come from `tmp/`-style scripts attached to #9291:

- **Class access.** Methods with `while $i < $n { $s = $!x; ... }` and the same loop without the
  read; `P.new` in a mainline loop.
- **`nqp::` ops.** One TRIR-accepted `sub b-<op>(int $n)` per op, called **by name**. A call
  through a `&code` value does not enter TRIR and measures the untyped path instead.

## 7. Out of scope, recorded

- Method bodies do not enter TRIR, so an empty `while` loop in a method is ~200 ns per
  iteration against rakudo's ~20. This is a separate issue.
- A call through a `&code` value (`&f($n)`) does not enter TRIR even when `f` is accepted.
