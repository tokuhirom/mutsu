# ADR-0040: Array and Hash elements are itemized at the *store*, not compensated at the read

- **Status**: Accepted (Slices 0-1 implemented; see "Implementation status" below)
- **Date**: 2026-08-20
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0013](0013-container-interior-mutability-cellvalue.md) §5 open question 3 / §7 (the
  "2c / Track B proper" deferral this finding was parked behind, and the refinement that released it),
  [ADR-0001](0001-gc-strategy-and-phasing.md) §7 (layer 3a shipped; the "fused with GC" rule is
  history), [ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md) §7 (the *aliasing*
  surface of the same Raku model — explicitly disjoint from this one),
  [ADR-0015](0015-native-backed-container-storage-and-repr-bodies.md) (native-backed storage, which
  bounds the perf question in §5.2), `todo/deep/element-itemization-lost-in-scalar-binding.md` (the
  originating finding), `news/2026-08/param-bind-itemization.md` (the bind-side half, shipped)

> Raku's model is that every `Array`/`Hash` element **is** a `Scalar` container, so an element handed
> out is one item and renders itemized (`$["a", "b"]`). mutsu stores elements bare and compensates in
> one renderer. This ADR decides to put the itemization at the element *store*, using the itemization
> primitive mutsu already has, and records the measurements that show the campaign is far smaller
> than the originating finding estimated.

---

## 1. Context

### 1.1 What Raku specifies

An `Array`/`Hash` element is a `Scalar` container. Two consequences are observable on every element
read, with no parameter binding involved:

- **It is one item in list context.** `sub takes(*@a) { @a.elems }; takes(@c[0])` is `1`, however
  many elements `@c[0]` itself holds.
- **It renders itemized.** `@c[0].raku` is `$["a", "b"]`, not `["a", "b"]`.

And the discriminator is the *source container*, not the syntax that reads it: a `List` literal's
elements are not containers, so `((1,2),(3,4))[0]` is a bare `(1, 2)`.

### 1.2 The bind-side half already shipped

`news/2026-08/param-bind-itemization.md` (2026-08-11) made a value bound to a plain `$`-sigiled
parameter itemized — for-loop pointy params, sub/closure positional and named params, map/grep block
params, placeholders — via `Interpreter::itemize_plain_scalar_param`
(`src/vm/vm_helpers.rs:34-44`), pinned by `t/param-bind-itemization.t`. That is why every "pointy"
row in the measurements below already agrees with raku. **The store-side half is what remains**, and
it is what this ADR decides.

### 1.3 The divergence, measured on `main` (52631889f, 2026-08-20)

Each row is a separate block so no earlier statement can contaminate it
(`tmp/elemitem/final.raku`; `sub takes(*@a) { @a.elems }`):

| # | program | raku | mutsu |
| --- | --- | --- | --- |
| 01 | `my @c = [<a b>],[<c d>]; @c[0].raku` | `$["a", "b"]` | `["a", "b"]` |
| 02 | `my %h = a => [1,2]; %h<a>.raku` | `$[1, 2]` | `[1, 2]` |
| 03 | `@c[0,1].raku` | `($["a", "b"], $["c", "d"])` | `(["a", "b"], ["c", "d"])` |
| 04 | `@c.head.raku` | `$["a", "b"]` | `["a", "b"]` |
| 05 | `@c.tail.raku` | `$["c", "d"]` | `["c", "d"]` |
| 06 | `@c.first(*.so).raku` | `$["a", "b"]` | `["a", "b"]` |
| 07 | `@c.sort.raku` | `($["a", "b"], $["c", "d"]).Seq` | `(["a", "b"], ["c", "d"]).Seq` |
| 08 | `@c.reverse.raku` | `($["c", "d"], $["a", "b"]).Seq` | `(…).Seq` bare |
| 09 | `@c.map({$_}).raku` | `($["a", "b"], $["c", "d"]).Seq` | `(…).Seq` bare |
| 10 | `@c.pairs[0].value.raku` | `$["a", "b"]` | `["a", "b"]` |
| 11 | `@c.Slip.raku` | `slip($["a", "b"], $["c", "d"])` | `slip(…)` bare |
| 12 | `takes(@c[0])` | `1` | `2` |
| 13 | `takes(%h<a>)` | `1` | `2` |
| 14 | `takes(@c.head)` | `1` | `2` |
| 15 | `for @c { takes($_) }` | `1` | `2` |
| 16 | `(my @z = @c[0]).elems` | `1` | `2` |
| 17 | `[@c[0]].elems` | `1` | `2` |
| 18 | `join('\|', @c[0])` | `a b` | `a\|b` |
| 19 | `my @a = 1,2; @a[0] = (7,8); @a[0].raku` | `$(7, 8)` | `(7, 8)` |
| 20 | `my @a = 1,2; @a.push([7,8]); @a[2].raku` | `$[7, 8]` | `[7, 8]` |
| 21 | `my @a; @a[3] = [7,8]; @a[3].raku` | `$[7, 8]` | `[7, 8]` |
| 22 | `my @a; @a.append([7,8],[9,0]); @a[0].raku` | `$[7, 8]` (2 elems) | `[7, 8]` (2 elems) |
| 23 | `my @a = (1..3),(4..6); takes(@a[0])` | `1` | `3` |
| 24 | `my @l := 1, (1,2), [3,4]; @l[1].VAR.^name, @l[2].VAR.^name` | `List Array` | `Scalar Scalar` |
| 25 | `@c.raku` — **the invariant, must NOT gain `$`** | `[["a", "b"], ["c", "d"]]` | same ✅ |

Row 25 is the only agreement, and it is the constraint the fix must preserve: an `Array`'s *own*
`.raku` de-itemizes its elements. (A `Hash`'s does not — `%h.raku` is `{:a($[1, 2])}` in raku —
which matters in §1.5.)

### 1.4 The mechanism already exists, in triplicate, and it is transparent

mutsu has no single "itemized" variant; it has three, unified behind one entry point
`Value::item()` (`src/value/value_methods_a.rs:378-395`):

| held type | representation | definition |
| --- | --- | --- |
| Array / List | an `ArrayKind` tag — `ItemArray` / `ItemList` | `src/value/mod.rs:921-935`; `ArrayKind::is_itemized/itemize/decontainerize` at `src/value/value_collections.rs:18/23/33` |
| Hash | a `bool` on the repr variant, `ValueRepr::Hash(Gc<HashData>, bool)` | `src/value/mod.rs:1116-1125`; `hash_is_itemized` / `with_hash_itemized` at `src/value/value_methods_a.rs:338/361` |
| everything else (Seq, Range, Set, Bag, Mix, scalars) | a wrapper, `ValueRepr::Scalar(Box<Value>)` | `src/value/mod.rs:1296-1298`; `Value::scalar` at `src/value/view.rs:268-272` |

Crucially, **itemizing an Array only flips the tag — the backing `Gc<ArrayData>` is shared**, so
itemization costs no allocation and no copy.

**And it is already transparent to everything that consumes an element.** The decisive measurement
(`tmp/elemitem/proxy.raku`, `tmp/elemitem/proxy2.raku`) writes the post-fix state by hand — `my @i =
$[1,2], $[3,4]` — and compares it against today's bare `my @b = [1,2], [3,4]` across 25 behavioural
probes: `.elems`, `[0].elems`, `[0][1]`, `.WHAT`, `.^name`, `.flat`, `map`, `grep`, `sort`,
`reverse`, `.Str`, `.gist`, `.raku`, `push`, `eqv`, hash values, nested index, element assign, `for`
body, `deepmap`, `.Bool`, `.defined`, `.kv`, `.pairs`, `join`. **All 25 behave identically**, and a
second run of ten cross-checks (`.raku`, `.gist`, `.Str`, `eqv` across the two shapes, `==`, hash
`eqv`, `.sort.raku`, `.map(*.List).raku`, `.pairs.Hash.raku`, `.flat.raku`) matches **raku exactly on
all ten**. The reason is in the code: every list-context flattening decision point already consults
`is_itemized()` — `flatten_into_slurpy` (`src/runtime/types/signature.rs:147-165`),
`flatten_value_for_slurpy` (`src/vm/vm_value_helpers.rs:583-600`), `value_to_list`
(`src/runtime/utils/list.rs:51-95`), `flat_val` (`src/builtins/functions/flat.rs:100-160`),
`exec_make_array_op` (`src/vm/vm_data_ops.rs:107`), `coerce_to_array`
(`src/runtime/utils/coerce_containers.rs:397-400`), `flatten_append_args`
(`src/runtime/mod.rs:62-86`), the QuantHash coercions (`src/builtins/quanthash_coerce.rs:72/265/565`)
— and both renderers already handle it (`src/builtins/methods_0arg/raku_repr.rs:359-386` emits the
`$`, `src/runtime/utils/gist.rs:183-193` deliberately drops it).

**This is the single most important input to the decision**: the originating finding sized the
campaign as "changes what is IN every array — a survey-sized campaign with its own fallout class".
Measured, the *fallout* class is empty on 35 probes. What is genuinely survey-sized is only the
enumeration of store sites (§4), not the consequences of storing itemized values.

### 1.5 The compensators already grown next to the gap — including a failed attempt at option A

The hash store is bare: `build_hash_from_items_with_key_coercion`
(`src/runtime/utils/coerce_containers.rs:256-341`) inserts `boxed_val.clone()` verbatim, with no
`item()` anywhere in the path. Two read-side compensations were grown on top of it instead, and they
disagree with each other:

- **A read-side itemization patch, limited to one kind and one opcode.**
  `Interpreter::itemize_hash_value` (`src/vm/vm_var_index_ops.rs:8-15`) wraps the value on a hash
  subscript read — but only `if matches!(v.view(), ValueView::Array(_, ArrayKind::List))`. Its doc
  comment states the right intent ("following Raku's rule that hash slot access returns values in
  item context"); the implementation covers one of the kinds, from two call sites (`:1078` Str key,
  `:1093` Int key).
- **A render-side patch.** `raku_hash_value` / `itemize_scalar_repr`
  (`src/builtins/methods_0arg/raku_repr.rs:403-441`) synthesizes the `$` when the *whole hash* is
  rendered.

The result is one value with three different answers depending on who asks:

```
my %h = k => (1,2,3), a => [1,2];
%h<k>.raku              # mutsu $(1, 2, 3)   raku $(1, 2, 3)   <- read patch fires (List)
%h<a>.raku              # mutsu [1, 2]       raku $[1, 2]      <- read patch misses (Array)
%h.values.sort.head.raku # mutsu [1, 2]      raku $[1, 2]      <- read patch not on this path
%h.pairs.sort[1].value.raku # mutsu (1,2,3)  raku $(1, 2, 3)   <- nor this one
%h.raku                 # mutsu {:a($[1, 2]), :k($(1, 2, 3))}  raku identical  <- render patch
```

**This is option A (§3) already tried, and it is the strongest available evidence against it.** The
patch is incomplete on two independent axes at once — the value kind, and which reader is asked —
and each axis needs a separate widening that the next reader re-opens.

There is also a **state-dependent** artefact. `.grep`/`for` promote elements to `ContainerRef` cells
(`array_slot_ref`, `src/value/value_methods_b.rs:94-134`), and a promoted element deconts through
`resolve_array_entry` (`src/vm/vm_var_ops.rs:147`) to something `.head` treats as one item. So:

```
my @c = [<a b>],[<c d>]; sub takes(*@a){@a.elems};
say takes(@c.head);   # 2
@c.grep({True});      # a pure read, no assignment
say takes(@c.head);   # 1   <-- changed by a read
say takes(@c[0]);     # 2   <-- and now disagrees with .head
```

An unrelated read silently changes an element's list-context arity, and two element readers on the
same array disagree. That is the shape of a defect that gets rediscovered as an unrelated-looking
bug, and it is the strongest argument that the property belongs in the store rather than in whichever
reader happens to be asked.

### 1.6 The discriminator experiment — why the source container, not the binding form

`tmp/elemitem/probe19.raku` varies the *source* of a `for` loop while holding the binding form fixed:

| source | `for … { takes($_) }` raku / mutsu | `for … -> $v { takes($v) }` raku / mutsu |
| --- | --- | --- |
| real `Array` (`my @c = [<a b>],[<c d>]`) | **1 / 2** ❌ | 1 / 1 ✅ |
| `List` literal (`((1,2),(3,4))`) | 2 / 2 ✅ | 1 / 1 ✅ |
| `List` of `Array` literals (`([1,2],[3,4])`) | 2 / 2 ✅ | 1 / 1 ✅ |
| `Seq` derived from a real Array (`@c.map({$_})`) | **1 / 2** ❌ | — |

Three things fall out, and each closes an option:

1. **The implicit topic is not the bug.** It already agrees with raku for every non-Array source. It
   diverges only when the source's elements *should have been* itemized. So there is no
   "itemize the implicit topic" patch to write — the topic is faithfully handing on what the store
   gave it.
2. **The discriminator is the source container**, exactly as ADR-0036 concluded independently for
   the aliasing surface: real mutable `Array`/`Hash` yes, `List`/`Seq`/literal no.
3. **Itemization propagates through Seq pipelines for free.** The `@c.map({$_})` row diverges *only*
   because `@c`'s elements were bare to begin with; the flag rides along on the copy. A store-side
   fix therefore needs no work in `map`/`grep`/`sort`/`reverse`/`head`/`tail`/`first`/`pairs`/`kv` —
   which is precisely the work a read-side fix would need.

### 1.7 One claim in the originating finding is misfiled

The finding lists "list-destructuring bind write-through" (`my (\a, \b) := my ($x, $y); a = 10`)
as an affected symptom. **It is not an itemization gap and this ADR does not cover it.** Measured:

```
my $x = 1; my \a := $x; a = 10; say $x;              # 10 in raku AND mutsu (fixed 2026-08)
my ($p,$q) = 1,2; my \c := $p; my \d := $q; c = 20;  # 20 in both
my ($x,$y) = 1,2; my (\a,\b) := ($x,$y); a = 10;     # raku: $x == 10;  mutsu: dies
```

`--dump-ast` shows why: the list form desugars to `my @__destructure_tmp__ = [$x, $y].list` followed
by `VarDecl { name: "a", expr: Index { target: ArrayVar("__destructure_tmp__"), index: 0 } }`. The
temp array holds *copies* of `$x`/`$y`, so no amount of element containerization in that temp could
ever reach `$x`. The fix is in the desugar — emit N single binds, each to its own RHS lvalue, which
is the form that already works — and it is a small, well-scoped ticket, not part of this campaign.
Its failure mode has also changed since the finding was written: it now dies with
`Cannot assign to a readonly variable (a) or a value` rather than silently no-opping.

---

## 2. Decision

**Itemize an aggregate value at the point it is stored into a real, mutable `Array` or `Hash`
element, using `Value::item()`, applied *after* the store site's own flattening decision. Element
reads are not changed. The container's own renderers and flatteners keep de-itemizing, and the
read-side compensators are deleted once the store carries the property.**

Four parts:

1. **Where.** Every path that puts a value into an element of a real `Array` (`ArrayKind::Array`,
   `Shaped`) or a `Hash`: element assign, autovivification, `push`/`unshift`/`append`/`prepend`/
   `splice`, list-assign construction (`coerce_to_array`), and real-container literal construction.
   Sources whose elements are *not* containers — `List`, `Seq`, `Range`, `Capture`, `Match`,
   immutable `Set`/`Bag`/`Mix` — are untouched, which is what keeps §1.6's agreeing rows agreeing.

2. **What.** Only values whose one-item-ness is observable: `Array`, `List`, `Hash`, `Seq`, `Range`.
   Measured negatives that must **not** be wrapped: `Pair` (`@a[i].raku` is `:a(1)` in both), `Set`
   (`Set.new(1,2)` in both), `Int`/`Str`/`Any`/`Nil`. This is not a taste judgement — `Value::item()`
   on a scalar is already a no-op, and the negative rows are pinned in §4 slice 0.

3. **After flattening, never before.** `push(1,2)` must still add two elements and
   `append((5,4))` must still add two; only the per-element result is itemized. Measured on raku:
   `@p.push([9,8])` → one element, itemized; `@p.push(7,6)` → two elements, bare;
   `@q.append((5,4))` → two elements, bare. The ordering is already right in the code —
   `flatten_append_args` (`src/runtime/mod.rs:62-87`) implements the one-arg rule and returns a
   `Vec<Value>` *before* any `items.extend`, and `exec_array_push_op`
   (`src/vm/vm_data_push_ops.rs:45`) pops one value and expands only a `Slip` — so the hook goes
   inside the per-element loop of each store site, not at its entry, and cannot change arity.
   `t/append-one-arg-rule.t` is the pin.

   **One counter-current to reverse:** `normalize_push_unshift_args`
   (`src/runtime/methods_mut.rs:187-203`) and `normalize_push_args_for_copy`
   (`src/runtime/methods_call_helpers.rs:346-358`) currently *strip* a `Scalar` wrapper on the way
   in. Under a store-side model that de-itemization is exactly backwards and must stop; it is the
   most likely source of a "the fix does nothing on this path" symptom in slice 1.

4. **Delete the compensators.** The `Hash`-renderer re-itemization (§1.5) and the hash-store
   List-only itemization become redundant once the store carries the property, and the
   `.grep`-changes-`.head` artefact (§1.5) stops being reachable because `.head` gets an itemized
   value either way.

### Why this direction

- **It is the only direction that propagates.** §1.6's Seq row is the proof: a value itemized once at
  the store stays itemized through `map`/`grep`/`sort`/`head`/`pairs` with no per-method work,
  because the flag rides on the copied `Value`. A read-side fix has to be re-derived at every one of
  those producers, and would still be defeated by the next one added.
- **The consumer surface is already itemization-transparent** (§1.4: 25/25 behavioural probes,
  10/10 raku-exact renderer probes). This is measured, not assumed, and it is the single fact that
  reduces the campaign from "survey-sized with its own fallout class" to "enumerate the store sites".
- **It removes two compensators and one state-dependence** rather than adding a third
  (§1.5). Under the project's gain/risk definitions, a property that a read can silently change is
  exactly the incomplete-analysis shape that goes flaky; moving it to the store makes it a fact about
  the container.
- **It reuses a shipped primitive at zero representational cost.** `ArrayKind::itemize()` flips a tag
  on a shared `Gc`; no allocation, no copy, no new `Value` variant, no NaN-box change.
- **It is the increment ADR-0013 §5 Q3 deferred**, taken on the *representation* surface, and it is
  disjoint from ADR-0036, which takes the *aliasing* surface. ADR-0036 §7 says so explicitly
  ("This ADR does not depend on it and does not advance it") — verified still true: promoting an
  element to a `ContainerRef` leaves `.raku` unchanged, because the read chokepoint deconts.

---

## 3. Options considered

| Option | Fixes reads (rows 01-18) | Fixes stores (19-23) | Propagates through Seq | Blast radius | Verdict |
| --- | --- | --- | --- | --- | --- |
| **Status quo (compensate in the Hash renderer)** | ✗ | ✗ | ✗ | — | Rejected — the defect, and it is already self-inconsistent (§1.5) |
| **A. Itemize at the element read chokepoints** (`resolve_array_entry` / `resolve_hash_entry`) | partial | ✗ | ✗ | small | **Rejected, with evidence: it is already in the tree and already incomplete.** `itemize_hash_value` (§1.5) is exactly this option for hashes, and it covers one `ArrayKind` from one opcode; four readers of the same value give three different answers. Structurally, those two functions are the chokepoint for `ContainerRef` decont, not a general element-read funnel — `.head`/`.sort`/slices/`.values`/`.pairs`/iteration read elements without touching them. `ValueView::Array(` is matched in ~1215 places and bare `items[i]` indexing in ~156, so "the read" has no single site. It also cannot reach §1.6's Seq row. |
| **B. Itemize at every element *producer*** (indexing, slices, and the ~15 list methods that hand an element out) | ✓ | ✗ | ✗ | large and open-ended | **Rejected.** This is the finding's own read-side option. Every producer must know its source is a real Array/Hash — which `.kv`-through-Seq has already lost — and the set is not closed: the next method added re-opens the bug. It is the "add a compensator per site" shape. |
| **C. Itemize at the element store (this ADR)** | ✓ | ✓ | ✓ | medium, enumerable | **Chosen** |
| **D. Make every element a first-class `ContainerRef` cell ("full 2c")** | ✓ | ✓ | ✓ | very large | Deferred, not rejected — same verdict as ADR-0036 option D. It subsumes C *and* the aliasing surface, but pays a cell per element at construction and forces every `ValueView::Array` consumer that inspects elements to decontainerize. C reaches the same observable semantics for the representation surface at a fraction of the cost, and — being measurable — is the honest first estimate of what D would cost. |

Option C and ADR-0036's option C are complementary, not competing: ADR-0036 promotes *the elements
that are handed out as lvalues* to cells; this ADR itemizes *the values that go in*. An element that
is both promoted and itemized is a cell holding an itemized value, which is exactly what
`t/container-cell-raku-render.t` already pins for the hash case.

---

## 4. Phasing

Each slice is independently landable and independently green.

0. **Slice 0 — pin the semantics.** `t/element-store-itemization.t` covering every row of §1.3 as a
   currently-failing expectation set (skipped/`todo`-marked so it lands green) **plus the
   invariants that must not move**: row 25 (`@c.raku` stays bare), §1.6's three agreeing source
   rows, the arity rows (`push(1,2)` → 2 elements, `append((5,4))` → 2 elements), and §2.2's
   negatives (`Pair`, `Set`, `Int` elements stay unwrapped). This is the acceptance oracle, and the
   invariant half is what prevents slices 1-2 from "fixing" the divergence by over-itemizing.
   The existing corpus is the regression net, and it splits into two groups worth naming, because
   they pull in opposite directions:

   - **Must stay green unchanged** — `t/param-bind-itemization.t` (the bind-side half),
     `t/hash-raku-itemization.t` and `t/container-cell-raku-render.t` (the render invariants, which
     must not gain a double `$`), `t/append-one-arg-rule.t` (the arity contract),
     `t/hash-itemization-flag.t`, `t/itemized-array-keys-values.t` (an `$[…]` is still an Array for
     `.keys`/`.values`/`.kv`/`.pairs`).
   - **The de-itemization consumers most likely to break first** —
     `t/positional-param-deitemizes.t`, `t/for-bind-typed-array-deitemize.t`,
     `t/flat-itemization.t`, `t/flat-itemization-depth.t`, `t/itemized-list-hash-coerce.t`,
     `t/classify-bucket-itemized.t`, `t/iterator-itemized-array.t`, `t/map-grep-itemized-arg.t`,
     `t/slice-single-element-itemize.t`, `t/hash-key-single-itemize.t`,
     `t/computed-index-single-itemize.t`, `t/hyper-nested-itemize.t`.

   Expect a handful of the second group to encode today's bare-element answer and need re-baselining
   against raku. Each such edit is a finding to record in the PR, not a licence to edit freely.

1. **Slice 1 — the mutation sites.** Element assign, autovivification, and
   `push`/`unshift`/`append`/`prepend`/`splice` for real `Array` and `Hash`. Rows 19-22 turn green.
   This slice is first because it is the smallest closed set and it exercises the after-flattening
   rule (§2.3) against the arity pins.

   The important discovery for scoping is that the element-assign store is *not* a narrow point —
   `exec_index_assign_expr_named_op_inner` (`src/vm/vm_var_assign_index_named.rs:256`) is ~3200 lines
   writing through dozens of `items_mut()[i] = …` sites, with `exec_index_assign_expr_nested_op`
   (`:2716`), `assign_into_nested_container` (`:3096`), `exec_index_assign_deep_nested_op` (`:3184`)
   and `exec_index_assign_generic_op` (`:3492`) beside it. **Hook the value once at the top of
   `exec_index_assign_expr_named_op` (`src/vm/vm_var_assign_element.rs:409`), before it is popped**,
   rather than at the stores. Two pre-dispatch fast paths bypass that entry and need their own hook:
   `try_shared_array_element_assign` (`src/vm/vm_var_assign_element.rs:98`) and
   `try_fast_hash_element_assign` (`:181`). Push/unshift/append/prepend/splice have a clean split —
   the VM fast path `exec_array_push_op` (`src/vm/vm_data_push_ops.rs:45`, stores at `:242`/`:274`)
   and the slow path in `src/runtime/methods_mut_dispatch.rs:732/744/778/811/940`.

2. **Slice 2 — the construction sites.** List-assign into `@a`/`%h` and real-container literal
   construction. Rows 01-18 and 23 turn green, because every downstream producer inherits the flag
   (§1.6.3). **This is the slice with the perf question** — see §5.2.

   `coerce_to_array` (`src/runtime/utils/coerce_containers.rs:371-409`) is the shared tail but **not
   the only entry**: each caller has pre-emptive arms that bypass it — `bind_positional_value`,
   `LazyList`, `infinite_int_range_to_lazy_array`, and the iterable-instance arm in
   `exec_set_local_op_inner` (`src/vm/vm_var_assign_set_local.rs:924-972`), and the LazyList arm in
   `exec_assign_expr_local_op_inner` (`src/vm/vm_var_assign_local.rs:141-155`); further callers at
   `src/vm/vm_exec_dispatch.rs:1190-1198`, `src/vm/vm_misc_scope.rs:152`/`:207`,
   `src/vm/vm_misc_assign.rs:183`, `src/vm/vm_var_assign_index_named.rs:2593`/`:2669`,
   `src/vm/vm_misc_codevar.rs:505`. The hash side funnels much more cleanly:
   `coerce_hash_var_value` (`src/vm/vm_var_assign_coerce.rs:157`) →
   `build_hash_from_items_with_key_coercion` (`src/runtime/utils/coerce_containers.rs:256`), whose
   two `map.insert` sites (`:281`, `:316`, `:341`) are the narrowest common store — plus
   `coerce_to_hash` (`:14`) and the `%(…)` literal ops `exec_make_hash_op` /
   `exec_make_hash_from_pairs_op` (`src/vm/vm_data_ops.rs:168`/`:190`). Array literals are
   `exec_make_array_op` (`src/vm/vm_data_ops.rs:4`) and `exec_make_array_no_flatten_op` (`:152`) —
   two hooks, both already doing a per-element match.

3. **Slice 3 — `.VAR` for non-container elements.** Row 24: `my @l := 1, (1,2), [3,4]` reports
   `Scalar` for every element where raku reports `Int`/`List`/`Array`. It is the same model seen
   from the reflection side — a `List`'s elements are *not* containers — and it is the natural place
   to state the discriminator once in code.

4. **Slice 4 — delete the compensators.** The `Hash`-renderer re-itemization and the hash-store
   List-only itemization (§1.5), once slices 1-2 make them redundant. Verify the
   `.grep`-changes-`.head` state-dependence is gone by re-running §1.5's four-line program.

5. **Slice 5 — sweep.** Re-run §1.3 and §1.6, delegate the full `make roast` to CI, and check the
   bench CI series for the §5.2 regression. Record the outcome in this ADR's "Implementation
   status", `git mv` the originating finding to `news/2026-08/`, and file §1.7's desugar fix as its
   own ticket.

---

## 5. Open questions (the forks for the deciders)

1. **Exactly which value kinds itemize?** Measured: `Array`, `List`, `Hash`, `Range` diverge today
   and must be wrapped; `Pair`, `Set`, `Bag`, `Mix`, and all scalars already behave as one item and
   must not be. `Seq` is untested as a stored element (`my @a = (1,2).Seq, …`) and needs one probe.
   *Recommendation: route everything through `Value::item()` and let its existing three-way dispatch
   decide, then pin the negatives in slice 0 rather than hand-maintaining a type list.*

2. **Does the per-element hook cost `my @a = @b`?** This is the one real perf risk, and it is
   concentrated in slice 2. `coerce_to_array`'s `ValueView::Array` arm
   (`src/runtime/utils/coerce_containers.rs:379-409`) already scans for a `ContainerRef` and, finding
   none, **shares the `Gc` rather than copying** — its own comment says so ("Only rebuild when a cell
   is actually present (common path keeps sharing the Arc, so there is no per-assignment cost)").
   Widening that scan's *predicate* to "…or an aggregate needing itemization" is free, but any **hit**
   forces the `Gc::new(items.iter().map(…).collect())` rebuild — so `my @a = @b` where `@b` is an
   array-of-arrays goes from a refcount bump to a full Vec allocation plus n `Value` clones. That is
   the cost to measure, and it is a real one for nested-data-heavy code.

   Three mitigations are already available in the code shape. `exec_make_array_op`
   (`src/vm/vm_data_ops.rs:14-140`) already does a per-element `match val.view()`, so `[ … ]`
   literals absorb the hook at zero marginal cost. The `nil_elems_to_any` rewrite passes
   (`src/vm/vm_var_assign_set_local.rs:984-991`, `src/vm/vm_var_assign_local.rs:163-170`) use the
   same scan-then-rebuild-only-if-hit pattern and *already* clone the `ArrayData`, making them the
   cheapest attachment point. And a store whose source cannot hold an aggregate — a native-backed
   `array[int]` (ADR-0015), a Range expansion — is provably a no-op, so it can short-circuit on kind.

   Note the Range arms (`:414-462`) build element-by-element already, so there is no bulk memcpy to
   protect there. *Recommendation: make a bench CI check part of slice 2's acceptance rather than an
   afterthought, and measure `my @a = @b` over an array-of-arrays specifically — the generic bench
   set will not exercise the rebuild.*

   **Caveat on any local number here:** the truncation in Q5 means a "300k element" measurement on
   this path actually moves 100k elements. Re-measure after that is fixed.

3. **Do slices 1-2 land before or after ADR-0036's slices?** They touch the same element slot from
   opposite directions (this ADR decides what goes in; ADR-0036 decides what comes out as an
   lvalue). *Recommendation: independent, either order.* Two couplings to watch:
   `array_slot_ref`/`hash_slot_ref` must not de-itemize the value they wrap in a cell
   (`t/container-cell-raku-render.t` already pins the hash half; slice 0 should add the array half);
   and `Value::hash_autovivify_cell` (`src/value/value_methods_a.rs:568-570`) deliberately returns
   an existing `Array`/`Hash` element **as-is** instead of boxing it, which is precisely where the
   itemization model and the cell model meet — whichever ADR lands second owns reconciling that arm.

4. **Does anything depend on an element being bare?** The 35 probes in §1.4 found nothing, but they
   are behavioural, not exhaustive over serializers. `JSON::Fast`, `is-deeply`, precompilation
   serialization, and the `nqp::` ops are the places a stray `$` could surface.
   *Recommendation: slice 2 lands behind the slice-0 pin and the batteries gate; a leaked
   itemization shows up as a deterministic wrong `.raku`/`.gist`, not as a flake.*

5. **Fix the 100 000-element truncation first.** While measuring §5.2 an unrelated, unrecorded bug
   surfaced on the very path slice 2 touches: `my @a = ^300_000; @a.elems` is `100000` in mutsu and
   `300000` in raku, and `@a[299_999]` is `(Any)`. `MAX_ARRAY_EXPAND = 100_000`
   (`src/runtime/utils/coerce_containers.rs:350`) is applied in the **finite** Range branch as well
   as the infinite one, so it is hard data loss, not a laziness threshold. The slurpy and
   slice-assign siblings truncate identically. Recorded as
   `todo/tickets/finite-range-assign-truncates-at-100k.md`. *Recommendation: fix it first, so slice
   2's bench numbers are measured against a store that actually stores everything.*

---

## 6. Consequences

- **Element reads become itemized everywhere at once**, including through `map`/`grep`/`sort`/
  `head`/`tail`/`first`/`pairs`/`kv`/slices/implicit topic, with no change to any of those
  producers.
- **Two compensators and one state-dependence are deleted** (§1.5), and `.head` and `[0]` stop being
  able to disagree about the same element.
- **More arrays hold itemized values.** This is the change with real blast radius, bounded by §1.4's
  measurement: the flag is already transparent to every consumer probed, and the failures it can
  produce are deterministic wrong renderings, not flakes.
- **Some existing `t/*item*.t` expectations encode today's bare answer** and will need re-baselining
  against raku. Each such edit is a finding about what mutsu got wrong, and should be called out in
  the PR rather than done silently.
- **`.raku` output changes for a lot of user-visible programs** — any nested data structure printed
  element-wise gains a `$`. That is correct (it is what raku prints), but it is the most visible
  effect and worth stating in `news/`.
- **If rejected / indefinitely deferred:** the 24 divergences in §1.3 stay, the Hash renderer keeps
  disagreeing with the hash element read, and the `.grep`-changes-`.head` artefact keeps making
  element arity depend on program history — which is the hardest class of bug to attribute from a
  failure message.

---

## 7. Adjacent findings — the same Raku model, different surfaces

"An Array/Hash element is a `Scalar` container" is one model with three consequences, and mutsu
approximates each separately. Recorded here so a future reader can see the whole shape:

- **This ADR — the representation surface.** One item in list context, `$` in `.raku`. Fixed by
  itemizing at the store.
- **[ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md) — the aliasing surface for
  pair producers.** `(@a[0]:p).value` must be the element's container, so writes go through and
  reads see later writes. Fixed by `array_slot_ref`/`hash_slot_ref` promotion. Disjoint from this
  ADR by measurement: a promoted cell deconts at the read chokepoint, so it changes neither `.raku`
  nor list-context arity.
- **`todo/deep/for-loop-rw-element-alias-lost-through-deferred-closure.md` — the binding-lifetime
  surface.** `for @a -> $v is rw` snapshots instead of aliasing, so an escaping closure writes a
  disconnected cell. Same primitive as ADR-0036, different consumer. That ticket's own note ("do not
  conflate the two") is correct and this ADR does not change it.
- **`todo/deep/element-itemization-lost-in-scalar-binding.md`'s third bullet** (list-destructuring
  bind write-through) belongs to none of the three — it is a desugar bug (§1.7) and should be
  re-filed as a ticket when this ADR's slice 5 retires the finding.

---

## 8. Implementation status (2026-08-21)

Slices 0-1 landed in full, including every mutation-site shape named in §2/§4's Slice 1
description (element assign, autovivification — both single- and nested-level — and
`push`/`unshift`/`append`/`prepend`/`splice` for a real `Array` or `Hash`).

- **Slice 0** (acceptance oracle): `t/element-store-itemization.t` — the full §1.3 divergence
  matrix (rows 01-25, dual-oracled against `raku`), the §1.6 agreeing-source rows, the §2.3 arity
  invariants (`push(1,2)` / `append((5,4))` stay 2 bare elements), the §2 negatives
  (`Pair`/`Set`/`Int` elements stay unwrapped), a native-array safety check, and a dedicated
  section for every mutation-site shape this slice fixes: single-index element assign
  (rows 19-22), the hash equivalent (`%h<k> = [1,2]`), `unshift`/`prepend`/multi-arg `push`, nested
  array/hash autovivification (`@a[5][0] = 1`, `%h<a><b> = 1`), `Hash.push`, and reference-shared
  push (`@a.push(@b)`) including that `@b` read directly stays bare while `@a[0]` is itemized, and
  that a later mutation of `@b` still propagates through the shared cell. 46 assertions total; rows
  01-18, 23 (construction-site itemization, Slice 2) and 24 (`.VAR` reflection, Slice 3) stay
  `todo`-marked.

- **Slice 1** (the mutation sites), in two composed layers:

  1. **`Interpreter::itemize_value`** (`src/vm/vm_run_loop.rs:942`), the pre-existing narrower
     sibling of `Value::item()` already shipped for the bind-side half (`itemize_scalar_store`, same
     shape — it rewrites `Array`/`Hash`/`Seq`/`Mixin` in place, leaving every scalar `ValueView`
     discriminant byte-identical), is applied at the dozens of individual per-element leaf-store
     sites inside the ~3200-line `vm_var_assign_index_named.rs` machinery (both fast paths in
     `src/vm/vm_var_assign_element.rs`, `native_store_val`'s computation, `hash_insert_through`
     calls, and the "autovivify a fresh Nil/missing container" branches) and at every push/append/
     unshift/prepend fast-path per-element store (`src/vm/vm_data_push_ops.rs`'s `ArrayPush` opcode,
     and the second, independent fast path `try_native_array_mut`/`native_array_storage_mut` in
     `src/vm/vm_call_method_mut_ops.rs` that serves the multi-arg/captured-closure call shapes
     `unshift` reaches exclusively — `unshift` has no dedicated VM opcode).
  2. **`Value::itemize_for_element_store`** (`src/value/value_methods_a.rs`), a new sibling gated on
     the ADR's own §2 "What" list (`Array`, `Hash`, `Seq`, and every `Range` shape — the one kind
     `itemize_value` does not cover, since nothing needed it for the bind-side half), delegates to
     `Value::item()` for those kinds and is a no-op otherwise. Used at the sites discovered/added
     during implementation that were not already covered by (1): the single hook at the top of
     `exec_index_assign_expr_named_op_seeded` (gated on a plain `Int`/`Str` index, so a slice
     assign's whole RHS list is never itemized as one unit); the two nested-autovivification
     construction sites in `exec_index_assign_expr_nested_op`
     (`vm_var_assign_index_named.rs`, both the array-outer and hash-outer arms); `hash_push_insert`
     (`src/runtime/methods_mut_hash.rs`, the `Hash.push`/`.append` chokepoint); `flatten_append_args`
     (`src/runtime/mod.rs`, the one-arg-rule choke point shared by ~13 append/prepend call sites) and
     `normalize_push_unshift_args`/`normalize_push_args_for_copy` (`src/runtime/methods_mut.rs`,
     `src/runtime/methods_call_helpers.rs` — see the reversal note below); and the discrete-element
     branch of `splice`'s replacement-argument loop (`src/runtime/methods_mut_dispatch.rs`).

  Both helpers apply itemization strictly *after* each site's own one-arg-rule/Slip-flattening
  decision, never before — `push(1,2)` still adds two bare elements, `append((5,4))` still flattens
  to two bare elements, and only the per-element result is itemized (§2 part 3, verified by the
  arity-invariant rows).

**The reversal `normalize_push_unshift_args`/`normalize_push_args_for_copy` needed** (§2 part 3's
"one counter-current"): both used to *strip* an incoming `Scalar` wrapper / itemized-`Array` kind on
the way into `push`/`unshift`, because under the old read-side-compensated model any incoming
itemization was a leftover to discard before storing. Under the store-side model that is exactly
backwards, so both now itemize the final per-element value instead of stripping it.

**Nested autovivification (`@a[5][0] = 1`, `%h<a><b> = 1`) is covered.** The freshly-autovivified
intermediate container itself becomes a real stored element of the outer array/hash, so it itemizes
the same way any other element store does — `@a[5].raku` is `$[1]` and `%h<a>.raku` is `${:b(1)}`,
matching raku. Deeper (3+-level) chained assignment
(`exec_index_assign_deep_nested_op`/`exec_index_assign_generic_op`) was not separately audited; no
acceptance row exercises it, and it is left for a follow-up slice if a gap surfaces.

**Reference-shared push (`@a.push(@b)`) needed a representation choice, not just a hook call.**
Naively itemizing the *cell's* contents (flipping the shared `ArrayData`'s own `ArrayKind` tag) was
tried first and is wrong: `@a[0]` and `@b` share the exact same `ContainerRef` cell, so mutating the
cell's own kind tag makes `@b` itself (read directly, not through the pushed element) also render
itemized — contradicting raku, where `@b.raku` stays bare while `@a[0].raku` is `$[1, 2]`. The fix
instead wraps the `ContainerRef` *itself* in an outer `Value::Scalar` (`Value::container_ref(cell)
.item()`, in `src/vm/vm_data_push_ops.rs`'s `value_source_idx` branch) — the same "wrap anything
that is not Array/Hash in a Scalar" arm `Value::item()` already has, so the shared cell's own content
is untouched and only the pushed element's own wrapper carries the itemization. This introduced a
new `Value::Scalar(Box(ContainerRef(_)))` shape that the pre-existing method-dispatch decontainerize
step (`src/vm/vm_call_method_ops.rs`, `exec_call_method_op_impl`) did not know about — it only
recognized a *bare* `ContainerRef` invocant — so `@a[0].elems` (and every other non-rendering method)
initially regressed to `1` instead of decontainerizing through to the aliased array's real element
count (caught by the pre-existing `t/native-callmethod-push.t` and `t/push-array-reference.t`, not by
this ADR's own pin file). Fixed by widening that same decont step to also see through a
`Scalar`-wrapped `ContainerRef`, **except** for `raku`/`gist`/`perl` (mirroring the pre-existing `VAR`
exception for a bare `ContainerRef`) — those three must keep the `Scalar` wrapper intact, since the
itemization they need to render lives only in that outer wrapper.

**A pre-existing, unrelated `splice` arity bug was found and filed separately, not fixed here**
(`todo/tickets/splice-multi-arg-array-incorrectly-flattens.md`): `splice`'s replacement-argument
handling flattens *every* `Array`/`List` argument unconditionally, where raku only flattens an
`Array`/`List` argument when it is the sole replacement argument (the same one-arg rule
`push`/`append`/`unshift`/`prepend` already implement correctly). This ADR's itemization hook is
applied to whatever `splice` already decides to keep as one element (which correctly reaches a
discrete `Range` argument, pinned in the acceptance test) without touching that unrelated flattening
decision, to keep this PR's blast radius scoped to itemization.

**A companion bug in `.pick`/`.roll`/`.head`/`.tail`'s generic fallback was found by CI**
(`roast/integration/advent2010-day11.t` died mid-run: `%next-step{$a ~ $b}.roll.key` threw "No such
method 'key' for invocant of type 'Hash'"). `value_to_list` (`src/runtime/utils/list.rs`) has two
call-site shapes that need opposite answers to the same question, "does this value expand to its
elements": when `val` is being flattened as an ELEMENT of some OTHER container (its primary, correct
job, and the one every store-site hook in this ADR relies on), itemization must stop it from
expanding — but `.pick`/`.roll`'s generic fallback (`dispatch_core_range.rs`) and `.head`/`.tail`'s
non-`Array` fallback call `value_to_list(target)` where `target` IS THE RECEIVER, to decompose it
into ITS OWN elements — a question itemization has no say in, since it was never itemized as an
element of anything here. A nested-autovivified `%next-step{$a~$b}` (now itemized by Slice 1)
exposed the gap: `value_to_list` treated it as one opaque item and `.roll` "rolled" the whole hash
instead of one of its pairs. Fixed with a new sibling, `value_to_list_for_receiver` (same file),
that strips the receiver's own itemization (`descalarize` + de-itemize `Array`/`Hash`) before
decomposing — used at every `value_to_list(target)` call in `dispatch_core_range.rs` where `target`
is confirmed to be the method's own receiver (left the two sites where `target` was already
narrowed to a bare `Range`/`GenericRange` by an outer match unchanged, since those can never be a
Scalar-wrapped receiver). `.roll`/`.pick` also gained a dedicated `Hash` arm for `.roll` (mirroring
the one `.pick` already had), rather than relying on the fallback at all for the common case.
Pinned in `t/element-store-itemization.t`'s new CI-regression section (itemized Hash/Array elements
via `.pick`/`.roll`/`.head`/`.tail`).

A `roast/integration/deep-recursion-initing-native-array.t` stack overflow observed locally under a
**debug** build is unrelated and pre-existing (reproduces identically on `main`, unaffected by this
PR); it passes under the **release** build CI actually uses for `make roast` (confirmed both on
`main` and on this branch), matching the documented debug-vs-release roast guidance.

**Verification**: `cargo clippy -- -D warnings` and `cargo fmt --check` clean. Full local `t/` suite
(3328 files) passes unchanged. Targeted whitelisted roast batches — `S32-array/*` (all 21
whitelisted files, including `push`/`unshift`/`splice`/`create`/`delete*`/`multislice-6e`),
`S32-hash/*` (all 17 whitelisted files), `S09-typed-arrays/*` (9 files, including the native/shaped
variants), `S02-types/{array,array_extending,array_ref,assigning-refs,autovivification,
flattening,hash,hash_ref,list,multi_dimensional_array}.t`, `S32-list/{pick,roll}.t`, and **all 119
whitelisted `roast/integration/*.t` files** (release build) — all pass.

---

*If the mechanism judgment changes later, supersede this ADR rather than rewriting it.*
