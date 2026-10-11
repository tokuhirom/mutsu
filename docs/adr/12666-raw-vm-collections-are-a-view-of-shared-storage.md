# ADR-12666: Raw VM collections are a distinct view of shared storage

- **Status**: Accepted (2026-10-11; maintainer accepted the proposal)
- **Date**: 2026-10-11
- **Issue**: [#12666](https://github.com/tokuhirom/mutsu/issues/12666)
- **Amends**: [ADR-11553](11553-nqp-representation-and-reference-identity.md) §3.1
  for collections (the mechanism, not the goal); §3.2 is unchanged.
- **Related**: [ADR-0005](0005-nanbox-representation-encoding.md) (8-byte `Value`, kind tags),
  [ADR-0015](0015-native-backed-container-storage-and-repr-bodies.md) (native storage, REPR
  claims), [ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md) (bare
  values), [#11553](https://github.com/tokuhirom/mutsu/issues/11553),
  [#12662](https://github.com/tokuhirom/mutsu/pull/12662) (BOOT scalars)

## 1. Question

ADR-11553 decided that a raw VMArray/VMHash and the high-level `List`/`Hash` that `nqp::hllize`
makes from it are different objects sharing element storage, and left the mechanism open ("a
new tag or wrapper is an implementation choice"). #12666 asks, before any implementation, for a
decision per op between:

- **A. Implement** the raw/HLL distinction, or
- **B. Record as "Not applicable"** in `docs/nqp-op-coverage.md`.

Ops: `bootarray`, `boothash`, `bootintarray`, `bootnumarray`, `bootstrarray`, `ishash`, and the
approximate `islist`. The maintainer's direction for this decision: **raise compatibility as far
as possible.**

## 2. Evidence

### 2.1 Who calls these ops

Scan of the latest release of every distribution in the fez index (1641 distributions,
2026-10-11; tarballs regex-scanned, not run):

| Op / pattern | Uses | Distributions |
| --- | ---: | ---: |
| `nqp::eqaddr` | 174 | 25 |
| `nqp::hash` / `nqp::list` (constructors) | 43 / 26 | 19 / 11 |
| `nqp::getattr(…, '$!storage' / '$!reified')` | 16 / 13 | 8 / 9 |
| `nqp::hllize` | 19 | 9 |
| `nqp::iscont` | 22 | 8 |
| `nqp::islist` | 4 | 3 |
| `nqp::bindattr` / `p6bindattrinvres` of `$!storage`/`$!reified` | 5 / 2 | 4 / 2 |
| `nqp::isrwcont` | 1 | 1 |
| `nqp::ishash`, `nqp::boot*array`, `nqp::boothash`, `nqp::iscont_i/n/s` | 0 | 0 |

The raw/HLL distinction is load-bearing in real code:

- **AttrX::Mooish** (`ClassHOW.rakumod:72,135`) and **GTK::Simple** (`Scheduler.rakumod:31`)
  branch on `nqp::islist($task)` to tell a raw task list from a callable, then
  `nqp::hllize` it. **Rakudo::Options** (`Options.rakumod:31`) does
  `nqp::if(nqp::islist($L), $L, nqp::list($L))` — with today's approximation a Raku `Array`
  takes the wrong branch.
- **Hash::int**, **Hash::str**, **Hash2Class**, **Cro::RPC::JSON**, **Terminal::Widgets**,
  **CoreHackers::NfaChainsaw** and **Rakudo::Options** build a raw `nqp::hash`/`nqp::list`
  and expose it through `nqp::hllize`, calling Raku methods (`.keys`, `.map`, `.raku`) only on
  the result.
- Eight to nine distributions each reach into `$!storage` / `$!reified` and mutate the raw
  storage expecting the HLL object to see it.

Nobody calls `ishash` or the five `boot*` collection type ops directly, but they are the
type-level face of the values the constructors above produce (`nqp::what(nqp::list())` *is*
`nqp::bootarray()`), so they fall out of the same representation at no extra cost. Option B
would leave the callers above with wrong answers; it is rejected (§5).

### 2.2 Rakudo observations (2026.09, `use nqp`)

In addition to ADR-11553 §2:

| Probe | Rakudo | mutsu today |
| --- | --- | --- |
| `nqp::list(1,2).^name`, `.REPR` | `BOOTArray`, `VMArray` | `List`, `P6opaque` |
| `nqp::hash().^name`, `.REPR` | `BOOTHash`, `VMHash` | `Hash`, `P6opaque` |
| `nqp::list_i(1).^name`; its `hllize` | `BOOTIntArray`; unchanged | `List`; `List` |
| `eqaddr($l, hllize($l))` | 0 | 1 |
| `eqaddr(hllize($l), hllize($l))` | 0 | 1 |
| `eqaddr(hllize($L), $L)` for an HLL `$L` | 1 | 1 |
| `bindpos`/`push`/`bindkey` on raw, read through `hllize` result | visible | visible |
| `islist`: raw, `hllize`d, `[1]`, `(1,2)`, `slip(1,2)`, `Seq` | 1, 0, 0, 0, 0, 0 | 1, 1, 1, 1, 1, 0 |
| `islist`: `my int @i`, `my str @s`, `Buf.new`, `blob8.new` | 1, 1, 1, 1 | 1, 1, 0, 0 |
| `ishash`: raw, `hllize`d, `{a=>1}`, `Map.new`, `OUR::` / its `$!storage` | 1, 0, 0, 0, 0 / 1 | dies |
| `islist`/`ishash` of `bootarray()`, `bootintarray()`, `boothash()` type objects | 1 | dies |
| `nqp::what(nqp::list())` `eqaddr` `nqp::bootarray()` | 1 | dies |
| `nqp::create(nqp::bootarray())`; `create(bootintarray())` then `push_i` | empty `BOOTArray`; works | dies |
| `nqp::istype(raw, Mu)`, `nqp::istype(raw, Any)`, `raw ~~ List` | 0, 0, False | 1, 1, True |
| raw `.HOW` | `KnowHOW` | `ClassHOW` |
| `raw.elems`, `.raku`, `.defined`, `"$raw"`, `for $raw {}` | `X::Method::NotFound` (invocant of type `BOOTArray`) | work |
| `raw[1]`, `$h<x>`, `+raw`, `?raw`, `my @a = raw`, `say raw` | work (argument hllized) | work |
| `sub ($x) { islist($x) }(raw)`; pointy block; method arg; `$x is raw` | 0 (hllized) | 1 |
| `sub () { raw }()` return; `my $s = raw`; `nqp::atpos(nqp::list(raw), 0)` | raw kept | — |
| `@a[0] := raw` then `islist(@a[0])` | 0 (hllized by the bind) | 1 |
| `(1, raw)[1].^name`; `hllize(list(list(1)))[0].^name` | `List`; `List` | `List`; `List` |
| `sub ($x) {}(nqp::list_i(1))` | type-check failure (`BOOTIntArray` is not `Any`) | binds |
| `getattr(@a, List, '$!reified')`: `islist`, twice `eqaddr`, `push` visible in `@a` | 1, 1, yes | 1, 1, yes |
| `p6bindattrinvres(create(List), List, '$!reified', $raw)`, later `push($raw)` visible | yes | no (copied) |
| `bindattr(%r, Map, '$!storage', $raw)`, later `bindkey($raw)` visible | yes | no (copied) |
| Capture `@!list` / `%!hash` via `getattr`: `islist` / `ishash` | 1 / 1 | 1 / dies |
| `nqp::elems(@a)` on an HLL Array | dies "does not support elems" | 3 |
| `nqp::istype(box_i(1, bootint()), Mu)` | 0 | 1 |

## 3. Decision

**A for all seven ops.** `bootarray`, `boothash`, `bootintarray`, `bootnumarray`,
`bootstrarray`, `ishash` and an exact `islist` are implemented. None is recorded as "Not
applicable". The mechanism refines ADR-11553 §3.1 as follows.

### 3.1 A raw collection is a second view of the same storage node

The object/storage split that ADR-11553 asks for already exists in mutsu at the right place:
`ArrayKind` (`List`, `Array`, itemized forms) lives in the **nanbox kind tag of the 8-byte
`Value`**, not in the shared `ArrayData`. `(1,2)` and the `Array` that shares its `Gc` differ
only in that tag. A raw VM collection is one more view of the same kind:

- Two new Gc-backed nanbox kinds, `VmArray` and `VmHash`, whose payload is the existing
  `Gc<ArrayData>` / `Gc<HashData>`. No new allocation, no shell node, no new field on
  `ArrayData`/`HashData` (the native element kind of `list_i/n/s` is already
  `ArrayData::nqp_elem`).
- *(Amended by §7: they view as `List` / `Hash`, and only the Rakudo-visible differences
  read the tag.)* They surface as **new `ValueView` variants** (`VmArray`, `VmHash`), not as an `ArrayKind`
  variant. The ~1600 `ValueView::Array` and ~900 `ValueView::Hash` match sites then do not
  match a raw value, which is exactly Rakudo's behaviour: a raw collection supports the REPR
  (`nqp::`) ops and nothing else. The nqp positional/associative ops reach the storage through
  one accessor that accepts both the raw and the HLL views, so each op is written once.
- `hllize` re-tags: `VmArray` with object elements → `ArrayKind::List` view of the same `Gc`;
  `VmHash` → `Hash` view. A native-element `VmArray` stays raw (Rakudo maps only the object
  types). An HLL operand is returned unchanged.

### 3.2 Constructors

Raw views are produced by `nqp::list`, `list_i/n/s`, `nqp::hash`, `nqp::create` of a BOOT
collection type, `nqp::clone` of a raw value (a new storage node), and the storage attributes
of §3.6. Everything else keeps producing HLL values.

### 3.3 Where a raw value becomes high-level

As in Rakudo, only at three places:

1. `nqp::hllize`;
2. **binding a Raku-level parameter** (sub, method, block, `is raw` and `is rw` alike) — one
   helper, a single tag test per bound argument, applied at every binder tier (VM fast path,
   signature binder, TRIR entry, JIT-compiled callee). Native-element raw arrays are not
   mapped, so they then fail an `Any` type check as in Rakudo;
3. reading an element through an HLL subscript of an HLL collection.

Assignment (`my $s = raw`), `:=`, `return`, and nqp op arguments/results keep the raw value.

### 3.4 What a raw value answers

`.^name`, `.WHAT`, `.REPR` and `.HOW` (a `KnowHOW`) answer through the meta level; every method
call dies with `X::Method::NotFound` naming the BOOT type; `nqp::istype(raw, Mu)` is 0;
boolification answers by element count as Rakudo does. Since the binder hllizes, every Raku
*routine* (`say`, `postcircumfix`, `prefix:<+>`, `infix:<,>`, `STORE`) keeps working on a raw
argument, matching the table in §2.2.

### 3.5 Identity

`nqp::eqaddr` compares the storage node **and** the view class (raw vs HLL): raw vs. its
`hllize` result is 0, two raw handles (or two `$!reified` reads) of one node are 1, and an HLL
value is `eqaddr` to itself through `hllize`. `.WHICH` of a raw value is a method call and
dies, as in Rakudo.

**One accepted divergence.** Rakudo allocates a fresh `List` wrapper per `hllize` call, so
`eqaddr(hllize($l), hllize($l))` is 0 and their `.WHICH`es differ; under this ADR both are 1 /
equal. Reproducing it needs an identity per HLL wrapper separate from its storage, i.e. the
shell rejected in §5 (an allocation and an indirection on every `List`/`Array`/`Hash`), or a
shell kind only for `hllize` results that every decode/re-encode path would have to preserve.
No surveyed code compares two separate `hllize` results of one raw collection. Revisit only
with such a caller.

### 3.6 Storage attributes

- `getattr(@a, List, '$!reified')`, `getattr(%h, Map, '$!storage')` and a Capture's
  `@!list`/`%!hash` return a raw view of the object's own node: O(1), shared, `eqaddr`-stable.
- `p6bindattrinvres(nqp::create(T), T, '$!reified' | '$!storage', $raw)` returns the HLL view
  (kind from `T`) of `$raw`'s node: O(1), shared afterwards — replacing today's O(e) copy.
- `bindattr` onto an **existing** HLL object cannot redirect the object's node (every holder of
  `%r` points at it), so it keeps copying; a later mutation through `$raw` is not seen through
  `%r`. This residual divergence is tracked in
  [#12672](https://github.com/tokuhirom/mutsu/issues/12672), which needs its own design.

### 3.7 BOOT type objects

`BOOTArray`, `BOOTHash`, `BOOTIntArray`, `BOOTNumArray`, `BOOTStrArray` are explicit type
objects with `KnowHOW` meta-objects outside `Mu`, REPR `VMArray` / `VMHash`. `nqp::what` of a
raw value is the matching one (`eqaddr` 1), `nqp::create` makes an empty raw value of that
element kind, and a type object passes its own REPR test. The BOOT scalar types that #12662
registered as `Any` subclasses move to the same mechanism
([#12670](https://github.com/tokuhirom/mutsu/issues/12670)).

### 3.8 `islist` and `ishash` (and the TODO in `nqp_ops_process.rs`)

The approximation goes. Following ADR-11553 §3.3, one REPR classifier answers all five type
tests:

- `islist` is 1 exactly for REPR VMArray: raw `VmArray` views of any element kind, the four
  `BOOT*Array` type objects, native `array[T]`, and `Buf`/`Blob`. `List`, `Array`, `Slip` and
  `Seq` are 0.
- `ishash` is 1 exactly for a raw `VmHash` view and `BOOTHash`. `Hash`, `Map` and a stash are 0.
- `isint`/`isnum`/`isstr` keep reading the BOOT scalar boxes (#12662).

### 3.9 REPR ops on HLL objects

Rakudo rejects the REPR ops on an HLL `List`/`Hash` (`nqp::elems(@a)`: "This type (Array) does
not support elems"); mutsu already does so for `atpos`/`bindpos` but not for the rest. To be
consistent with §3.1 the remaining positional and associative ops follow, through one
capability check, after the raw views exist so that callers can use `$!reified`/`$!storage`
([#12671](https://github.com/tokuhirom/mutsu/issues/12671)). Native arrays and `Buf`/`Blob`,
being VMArray REPR, keep working.

### 3.10 Native and rw reference tests

`iscont_i`, `iscont_n`, `iscont_s` and `isrwcont` keep the mechanism ADR-11553 §3.2 decided
(Accepted); this ADR does not change it. They are filed as their own `todo:deep` issue,
[#12673](https://github.com/tokuhirom/mutsu/issues/12673), so closing the collection half of
#11553 does not drop them. One real caller (`annotations`, `isrwcont($!value)`) needs the
attribute operand form.

## 4. Cost

- **`Value` stays 8 bytes.** 72 of the 104 nanbox kind slots are in use; two more fit. The new
  kinds join the Gc-backed range and reuse the existing `ArrayData`/`HashData` trace and drop
  arms.
- **`ArrayData`/`HashData` sizes are unchanged** (no new field; the `which_id.rs` size pin holds).
- **JIT**: the `vm_jit*` tiers contain no array/hash tag checks — collections go through helper
  calls — so no JIT assumption changes. TRIR's nqp lowering of `islist`/`ishash`/`hllize` calls
  the shared classifier.
- **`size_of::<OpCode>()`**: unchanged — the ops go through the existing `NqpOp` tables; no new
  opcode is needed.
- **Hot paths**: ordinary `List`/`Array`/`Hash` code is untouched (it never sees the new view
  variants). The one added cost is the binder's tag test per bound argument (§3.3); it is
  measured per the `perf-tuning` skill in the slice that adds it, and that slice lands only with
  no regression beyond noise on the call-heavy benchmarks.
- **Faithfulness risk**: a mutsu-internal path that passes a raw value to Raku-level code
  without binding through a parameter would now see an object without methods. The Rakudo
  matrix in §6 plus the full roast/`t/` gate catch that deterministically; it is fixed forward,
  never by making raw values answer methods.

## 5. Rejected alternatives

- **B — "Not applicable".** The ops are reachable, Rakudo gives meaningful answers, and §2.1
  shows callers whose behaviour depends on them (Rakudo::Options takes the wrong branch today).
- **A raw flag on `ArrayData`/`HashData`.** Raw and HLL views share the node and must answer
  differently (ADR-11553 §4).
- **An `ArrayKind::Vm` variant inside `ValueView::Array`.** Every wildcard arm of the ~1600
  match sites would silently treat a raw list as a `List` — methods, iteration, `.raku` — the
  lax behaviour Rakudo does not have. A separate view makes each supported operation an
  explicit opt-in. Hashes have no `ArrayKind`-like tag at all, so they would need the separate
  view anyway.
- **An object shell for every HLL collection** (the literal reading of ADR-11553 §3.1). An extra
  allocation and pointer chase on every `List`/`Array`/`Hash` operation, plus GC tracing of a new
  node type, to buy only §3.5's two-`hllize` identity and #12672's bind-onto-existing sharing.
- **A shell kind only for `hllize` results.** Those values would lose their shell at every
  `ValueRepr` decode/re-encode site, so identity would change silently after an unrelated
  mutation — a flaky-shaped bug for the same small gain.
- **Keeping the `islist` approximation and documenting it.** It answers 1 for `Array`, `List`
  and `Slip` and 0 for `Buf`, all opposite to Rakudo, and contradicts the four exact sibling
  tests.

## 6. Delivery and acceptance

Each slice merges green on its own; the Rakudo answers in §2.2 are the acceptance matrix, pinned
in `t/vm/` tests (raw/HLL names and REPRs, `eqaddr`, shared mutation, binder hllize per binder
tier, assignment keeping raw, method calls dying, BOOT type objects and `create`,
`islist`/`ishash` over native arrays, `Buf`, `Slip`, `Map` and stashes).

1. [#12668](https://github.com/tokuhirom/mutsu/issues/12668) (`todo:deep`): the `VmArray` /
   `VmHash` kinds and views, the constructors, the five BOOT collection type objects, `hllize`,
   binder hllization, the shared REPR classifier with exact `islist`/`ishash`. Registers the
   six missing ops (`islist` is already registered); `scripts/nqp-op-coverage.py` then shows
   Type / Conversion at 49/53.
2. [#12669](https://github.com/tokuhirom/mutsu/issues/12669) (`todo:ticket`): `$!reified`,
   `$!storage` and Capture storage as raw views; O(1) shared `p6bindattrinvres`.
3. [#12670](https://github.com/tokuhirom/mutsu/issues/12670) (`todo:ticket`): BOOT scalar
   types as `KnowHOW` types outside `Mu`.
4. [#12671](https://github.com/tokuhirom/mutsu/issues/12671) (`todo:ticket`): REPR ops reject
   HLL `List`/`Hash` operands.
5. [#12672](https://github.com/tokuhirom/mutsu/issues/12672) (`todo:deep`): `bindattr` onto an
   existing HLL object shares the raw storage (design first).
6. [#12673](https://github.com/tokuhirom/mutsu/issues/12673) (`todo:deep`): `iscont_i/n/s`,
   `isrwcont` per ADR-11553 §3.2 — the last four Type / Conversion ops.

No op is marked implemented before its slice delivers the Rakudo answers.

## 7. Amendment (2026-10-11, maintainer-approved): the raw tag is read, not matched

Implementing §3.1 showed that its second bullet does not fit mutsu. Rakudo's raw collections
reach `say`, `postcircumfix:<[ ]>`, `prefix:<+>`, `infix:<,>` and the other core routines
*through a parameter binder*, which hllizes them (§2.2). mutsu implements those routines
natively — opcodes and Rust builtins that take a `Value` — so there is no binder to do it. A
separate `ValueView` variant would have to be accepted at every one of the ~2500
`ValueView::Array`/`ValueView::Hash` consumers, and each one missed would answer wrongly for a
raw operand that Rakudo handles (`raw[1]`, `say raw`, `+raw`).

So the two kinds stay (§3.1's first bullet), but **a raw value views as the high-level `List` /
`Hash` over the same node**: every native builtin sees it exactly as Rakudo's binder would hand
it over. The places where Rakudo tells raw and high-level apart read the tag instead
(`Value::is_vm_array` / `is_vm_hash`):

- the REPR classifier (`islist`, `ishash`) and the type questions — `.^name`, `.WHAT`,
  `.REPR`, `.HOW`, `nqp::what`, `nqp::istype` and `~~` against a type (`runtime::boot_collection`);
- method dispatch, at the `CallMethod` / `CallMethodMut` opcodes: a raw invocant dies with
  `X::Method::NotFound` naming its `BOOT*` type;
- `nqp::eqaddr` (raw vs. high-level over one node is 0);
- the hllization points of §3.3: the signature binder, the positional-light call path, the
  TRIR entry and the method-call argument list, and the element a Raku subscript reads;
- `nqp::hllize`, `nqp::clone`, `nqp::create` and `.item` (a raw value has no itemized form).

This is not §5's rejected `ArrayKind::Vm`: no `ArrayKind` gains a variant, and the
wildcard arms that §5 worried about now give the hllized answer *on purpose*. The cost of the
trade is the other direction: an operation Rakudo implements as a method call on the raw
object, and so rejects, can succeed in mutsu when a native builtin performs it — `for raw {}`
and `"$raw"` iterate and stringify instead of dying. That is lenient, never a wrong answer
for a program Rakudo runs, and is tracked with the other leniencies in
[#12689](https://github.com/tokuhirom/mutsu/issues/12689).

## 8. Implementation status

- **#12668 (slice 1) — delivered.** `Kind::VmArray` / `Kind::VmHash` and
  `ValueRepr::VmArray` / `VmHash` (`src/value/vm_collection.rs`); `nqp::list`, `list_i/n/s`,
  `hash`, `create` and `clone` build raw values; the five `boot*` collection ops, `ishash` and
  an exact `islist` are registered (`src/runtime/boot_collection.rs`); `hlllist` / `hllhash`
  answer `BOOTArray` / `BOOTHash`. `t/vm/nqp-vm-collection-repr.t` pins the §2.2 matrix
  (it passes unchanged under Rakudo 2026.09).
- **#12669 (slice 2) — delivered.** `nqp::getattr` of `$!reified` / `$!storage` answers the
  storage view of the collection's own node (`Value::raw_storage_view`): a raw `VMArray` for a
  `List`, a raw `VMHash` for a `Map`/`Hash`, and for an `Array` an `IterationBuffer` — a third
  kind, `Kind::VmIterBuf`, because Rakudo's Array storage is an `IterationBuffer` (an `Any`
  with methods), not a `BOOTArray`. A Capture's `@!list` / `%!hash` are raw too.
  `p6bindattrinvres` of a raw collection into a fresh wrapper adopts the raw node in O(1)
  (`Interpreter::adopt_raw_storage`), carrying the wrapper's element/declared type and default
  onto it. `t/vm/nqp-storage-raw-view.t` pins it against Rakudo. Out of scope and filed:
  Scalar containers in an Array's reified slots (#12698), a stash's `$!storage` (#12699), and
  the two `IterationBuffer` representations (#12700).

## 9. Amendment (2026-10-11): `bindattr` onto an existing object rebinds the raw handle (#12672)

§3.6 left `bindattr` onto an **existing** HLL object copying, because every holder of `%r`
points at the object's node and cannot be redirected to `$raw`'s. The two candidate designs in
#12672 both fail on cost or exactness: a forwarding node adds a check to every access of every
collection, and rebinding the *target* variable is exact only when the target is a variable
(`bindattr(self, ...)` has none) and leaves every alias of it on the old node.

The decision is to move the redirection to the **other side**. The object's node is the one
many holders share and must stay put; the raw handle is almost always a fresh `nqp::hash` /
`nqp::list` held by one variable (or by nothing, as an operand). So:

- The contents still move into the object's node (O(e), as today).
- When the handle operand is a plain scalar variable and the object operand a plain variable,
  the compiler lowers `nqp::bindattr(OBJ, T, '$!storage' | '$!reified', $raw)` to the bind
  followed by `$raw := nqp::getattr(OBJ, T, <same attribute>)` — the raw view of the object's
  own node that §3.6 already defines. `$raw` and the object then share one store, so a write
  through either is seen by both. Cost: one extra `getattr` and a rebind; no runtime change.

Residual divergence, accepted: a handle held anywhere other than that variable (an attribute,
a list element, a closure's second capture) keeps the detached node it had, and a bind whose
object operand is not a plain variable keeps the plain copy. The first is exactly what
`$raw := ...` aliasing already is in Rakudo's own compiler output for an NQP local; no surveyed
caller (§2.1) mutates the handle through a second holder. The `IterationBuffer` instance
representation (#12700) is a separate question that this amendment does not change.
