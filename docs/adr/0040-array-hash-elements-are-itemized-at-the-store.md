# ADR-0040: Array and Hash elements are itemized at the *store*, not compensated at the read

- **Status**: Accepted (Slices 0-4 implemented; see "Implementation status" below)
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

## 8. Implementation status (2026-08-21; slice 2 added 2026-08-27; slices 3-4 added 2026-09-01; slice 4b 2026-09-02)

Slices 0-4 landed. Slice 4 landed its *store* half in full and left the compensator deletion
blocked on a newly measured class — see its section for the numbers. Slice 1 covered every mutation-site shape named in §2/§4's Slice 1
description (element assign, autovivification — both single- and nested-level — and
`push`/`unshift`/`append`/`prepend`/`splice` for a real `Array` or `Hash`). Slice 2 covered the
construction sites, turning §1.3's rows 01-18 and 23 green. Slice 3 covered the reflection side,
turning row 24 green — **every row of §1.3 now agrees with raku**, and nothing in the acceptance
oracle is `todo`-marked any more.

- **Slice 0** (acceptance oracle): `t/element-store-itemization.t` — the full §1.3 divergence
  matrix (rows 01-25, dual-oracled against `raku`), the §1.6 agreeing-source rows, the §2.3 arity
  invariants (`push(1,2)` / `append((5,4))` stay 2 bare elements), the §2 negatives
  (`Pair`/`Set`/`Int` elements stay unwrapped), a native-array safety check, and a dedicated
  section for every mutation-site shape this slice fixes: single-index element assign
  (rows 19-22), the hash equivalent (`%h<k> = [1,2]`), `unshift`/`prepend`/multi-arg `push`, nested
  array/hash autovivification (`@a[5][0] = 1`, `%h<a><b> = 1`), `Hash.push`, and reference-shared
  push (`@a.push(@b)`) including that `@b` read directly stays bare while `@a[0]` is itemized, and
  that a later mutation of `@b` still propagates through the shared cell. 46 assertions total; rows
  01-18, 23 (construction-site itemization, Slice 2) and 24 (`.VAR` reflection, Slice 3) started
  `todo`-marked and were un-marked by their own slices.

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
(since fixed — `news/2026-08/splice-replacement-arg-one-arg-rule.md`): `splice`'s replacement-argument
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

### Slice 2 (2026-08-27) — the construction sites

Two new primitives, both siblings of the ones slice 1 introduced:

- **`Value::needs_element_itemization`** (`src/value/value_methods_a.rs`) — "would
  `itemize_for_element_store` actually change this value?". Every construction-site hook scans a
  whole element vector with it *before* touching anything, which is what keeps §5.2's cost at
  zero on the common path (see "Perf" below). It excludes `Shaped`/`Lazy` arrays, on which
  `ArrayKind::itemize()` is already a no-op.
- **`Value::deitemize_element`** (same file) — the inverse, for the readers that hand out an
  element's *value* rather than its container.

And one container-level helper, **`itemize_real_array_elements`**
(`src/runtime/utils/coerce_containers.rs`): itemizes every element of a value that is a *real*
`Array` (`ArrayKind::{Array, Shaped, ItemArray}`), and is a no-op for a `List`/`Seq`/`Lazy`
result — which is exactly what keeps §1.6's agreeing rows agreeing and `((1,2),(3,4))[0]` a bare
`(1, 2)`. It scans first and only then `Gc::make_mut`s.

Hook sites:

- **Array side.** `coerce_to_array` was split into a private `coerce_to_array_inner` plus a
  public wrapper that applies the helper to its result — one hook covering every arm (the
  `Array`, `Seq`/`Slip`, `Hash`-flatten, `Set`/`Bag`/`Mix`, `Range` and catch-all arms) and every
  one of its ~15 callers. §4 warned that `coerce_to_array` is "not the only entry", and that held:
  the `@`-assign entry points reach it only on their *generic* arm, so a second hook went at
  `exec_set_local_op_inner` / `exec_assign_expr_local_op_inner`, immediately after
  `decay_nil_elements_for_var_assign` and **under the same `!is_bind` guard** — which covers the
  reified-`gather` `LazyList` arm, the iterable-instance reify arm and the infinite-range arm in
  one place. (The `!is_bind` guard is load-bearing: `my @l := 1, (1,2), [3,4]` must keep bare
  elements — row 24's model seen from the value side.)
- **Array literals.** `exec_make_array_op` and `exec_make_array_no_flatten_op`
  (`src/vm/vm_data_ops.rs`), applied to the finished value right after
  `decay_nil_container_elements` — so *after* each site's own one-arg-rule/Slip flattening
  decision, never before (§2 part 3). Free there, as §5.2 predicted: the value's `Gc` is
  freshly built with refcount 1, so `make_mut` never copies.
- **Hash side.** As §4 predicted, this funnels cleanly. A single `hash_stored_value` helper
  (`decay_nil_hash_value` composed with `itemize_for_element_store`, in that order — a decayed
  `Nil` becomes `Any`, which never itemizes) is applied at every `map.insert` value site in
  `coerce_to_hash` and `build_hash_from_items_with_key_coercion`, plus the `%(…)` literal ops
  `exec_make_hash_op` / `exec_make_hash_from_pairs_op`.
- **`.Array` / `.Hash` coercion.** `.Array` builds a real `Array`, so
  `((1,2),(3,4)).Array[0].raku` is `$(1, 2)` in raku; the `"list" | "Array"` arm's shared `wrap`
  closure now itemizes when `want_array`, and the three arms that were hand-rolling
  `Value::real_array(...)` beside it were folded back onto `wrap`. `.Hash` needed nothing — it
  already routes through `build_hash_from_items`.
- **JSON decoding** (`Parser::finish_object` / `finish_array`, `src/runtime/json.rs`). A decoded
  JSON object/array is a real `Hash`/`Array`, so `from-json('{"a":[1,2]}')<a>.raku` is `$[1, 2]`
  in raku. mutsu's native `JSON::Fast` provider builds those containers directly and so bypassed
  every other hook; this was a *pre-existing* divergence that slice 2 simply made conspicuous
  (every other container had grown the property). The `:immutable` forms decode to `Map`/`List`,
  whose elements are not containers, and are deliberately left alone. `to-json` round-trips
  unchanged either way.

**The `.List` counter-current — the slice-2 twin of slice 1's `value_to_list_for_receiver`
trap.** §4's note that "receiver decomposition vs element flattening" would resurface was
correct. Measured on raku:

```
my @c = [1,2],[3,4];
@c.list[0].raku       # $[1, 2]     .list keeps the containers
@c.list[0].VAR.^name  # Scalar
@c.List[0].raku       # [1, 2]      .List hands out each element's VALUE
@c.List[0].VAR.^name  # Array
($[1,2],).List[0].raku  # $[1, 2]   .List on a List is identity
```

So `.List` on a *real* `Array` decontainerizes, and `.List` on a `List` does not. Before slice 2
that divergence was invisible for a `my @c = …` array (its elements were bare) but already
reproduced for a slice-1-itemized one (`my @c; @c[0] = [1,2]; @c.List[0].raku`); slice 2 would
have made it visible everywhere. Fixed by de-itemizing in the `"List"` arm's `ValueView::Array`
branch, gated on `ArrayKind::Array`.

**Seventeen counter-currents, all of the same shape.** Every one is a site that asks a question
*about the value* while holding something that is itemized *because it is an element*. This is
the slice-2 recurrence of slice 1's `value_to_list_for_receiver` discovery, and it is the honest
cost of this slice: the hooks themselves were ~40 lines, the counter-currents were the work. Six
were found by the local `t/` suite; two more (`[Z]`/`[X]` and `.Array`) came out of a deliberate
Q4 sweep of serializers and receiver-decomposing methods (`to-json`/`from-json`, `is-deeply`,
`eqv`, `.WHICH`, `.Str`/`.gist`/`.join`, `zip`/`X`/`roundrobin`, the reduce metaop, and the whole
`.Array`/`.List`/`.list`/`.Slip`/`.Seq`/`.Bag`/`.Set`/`.Hash`/`.cache`/`.flat`/`.values`/`.keys`/
`.rotor`/`.sum`/`.reverse`/`.sort`/`.min` family on an itemized receiver) — 30 + 30 programs, each
dual-oracled. **The remaining nine only surfaced against roast, in eight different subsystems,
and across two separate iterations** — a targeted sweep chosen by "who consumes the code I
changed" found four, and the full `make roast` found five more (including the two that aborted a
whole file mid-run and the 88-failure `S03-sequence/exhaustive.t`).

That distribution is the reusable lesson, and it is stronger than ADR-0036's version of it: for a
change that alters *what is in every container*, the consumer surface is the whole language, so a
targeted roast sweep is **not** an adequate proxy for the full suite. Run `make roast` locally
before pushing rather than iterating through CI.

| site | symptom | fix |
| --- | --- | --- |
| `.antipairs` (`positional_antipairs`) | `@c.antipairs` gave `($[1,2] => 0,)` where raku gives `([1,2] => 0,)` | de-itemize the **key**. Rakudo's `.antipairs` is `self.pairs.map: *.antipair`, and `Pair.antipair` *reads* `$!value` to build the new key — an attribute read decontainerizes. The same element is therefore itemized as a pair's *value* (`@c.pairs.raku` is `(0 => $[1, 2],)`) and bare as a pair's *key*. |
| `.invert` (`extend_inverted_pairs`) | `{a => (1,2)}.invert` stopped expanding the value into one pair per member | de-itemize the value before decomposing it — the same reason, `Pair.antipair` reads `$!value`. |
| `.raku` on an array holding a `:=`-bound element (`raku_value_as_element`) | `my @a = {p=>1},{q=>2}; my $w := @a[0]; @a.raku` gave `[${:p(1)}, {:q(2)}]` — the bound element and its un-bound sibling disagreed | the row-25 de-itemization has to see **through** ADR-0036's `ContainerRef` element cell, which it did not. Caught by `t/container-cell-raku-render.t` and `t/element-bind-cell.t`. |
| `deepmap`/`nodemap`/`duckmap` (`deepmap_element_is_leaf`) | `%(a => 1, b => (2..3)).deepmap(*+1)` treated the itemized `Range` as a *leaf* | the leaf-vs-descend test is about what the value IS; descalarize first. The *result*'s itemization is decided separately by `itemize_result`, so it still answers `$(3, 4)`. |
| destructuring sub-signature `@`/`%` params (`bind_sub_signature_from_value`) | `-> [@a, $b]` given `[(1,2).Seq, 9]` failed the Positional check with `Scalar(Seq)`; **`Digest::RIPEMD` stopped working** (`-> [&f, $r, @K, $s]`) | binding an element to an `@`/`%` parameter reads the element's *value* — the same rule as `my @a := @c[0]`, which is `[1, 2]` and not `[[1, 2],]`. |
| `splice` on an element receiver (`scalar_holds_real_array`) | `@w[0].splice(*-2, 1)` resolved `*-2` against the wrong length | the gate matched `ArrayKind::Array` only, so an `ItemArray` receiver skipped the whole `@`-array block. An itemized array is still a real array *as a receiver*; the itemization describes how it behaves as an element of `@w`. |
| the reduce metaop (`exec_reduction_op`) | `my @m = [1,2],[3,4]; [Z] @m` gave `(($[1,2], $[3,4]),)` where raku gives `((1, 3), (2, 4))` | a reduction's operands are the element *values*, so an element decomposed out of the source list is handed to the operator decontainerized. Note this is genuinely different from the explicit infix form: raku's `@m[0] Z @m[1]` really is `(($[1,2], $[3,4]),)`, because there each operand IS an element. Guarded on `len() > 1` so the one-arg rule (which deliberately keeps a lone itemized operand whole — `[+] @m[0]` is `2`) is untouched. |
| `.Array` on an itemized receiver | `@a[0].Array.raku` gave `$[1, 2]` where raku gives `[1, 2]` | `.Array` builds a *new* real Array, which is not an element of anything, so the receiver's own itemization is dropped — exactly as the neighbouring `.list` arm already dropped it. The new array's own elements still itemize. |
| set-op membership (`set_contains`) | `my @e = 2, 1..2; @e[0] (elem) @e[1]` was False (`roast/S03-operators/set_elem.t`, 30 subtests) | the **container** is the receiver of the membership test, so its own itemization is stripped. The **needle** is deliberately left alone: a `Set`'s members keep their itemization in raku (`Set.new($[1, 2])`), so `.WHICH` membership must see exactly what was stored. |
| `.Map` (`map_hash_coerce::to_map`) | `%h.Map<a>.raku` gave `$[1, 2]`; `Foo.new(\|%args.Map)` bound `@.a` to one itemized array (`roast/S32-hash/map.t`) | a `Map`'s values are *not* containers, and the existing decont there only unwrapped a `Scalar` — it had to cover the kind/flag form too. |
| `is-deeply` (`seq_to_list`) | `is-deeply (1,2).Seq, $((1,2).Seq)` failed even though `eqv` says True (`roast/S02-types/pair.t`'s `Pair.invert` subtest) | `is-deeply` normalizes a `Seq` to a `List` before comparing; it has to see through the wrapper to find the Seq, or one side becomes a `List` and the other stays a `Scalar(Seq)`. |
| `.toggle` (`dispatch_toggle`) and `<>` (`__mutsu_zen_angle`) | `my @t = %(),; @t[0].toggle` yielded one element instead of the empty `Seq`; `($%h)<>.raku` was `${}` (`roast/S32-list/toggle.t`) | `.toggle` decomposes its own receiver, so it moved onto `value_to_list_for_receiver`. `<>` already cleared an itemized `ArrayKind` but not the Hash flag. |
| smartmatch (`smart_match_inner`) | `my @t = [<42+0i>, 10..50],; $t[0] ~~ $t[1]` was False (`roast/S02-types/range.t`, `roast/S03-smartmatch/range-range.t`) | raku's protocol is `$matcher.ACCEPTS($topic)`, and both invocant and argument decontainerize on the way in — so a `Scalar` wrapper is transparent on **both** sides, exactly as the pre-existing `ContainerRef` LHS unwrap already was. |
| the `...` sequence operator (`eval_sequence` / `eval_chained_sequence`) | `roast/S03-sequence/exhaustive.t`, **88 failures** — its seed table is `my @tests = …, [(1/4,1/2,1),(8,9)], …` fed through `infix:<...>(\|seed)` | `...` *decomposes* its seed operand into deduction seeds. A pre-existing divergence (`$(1,2) ... 10` was already wrong) that slice 2 made reachable through elements. |
| `.pairup` and the whole n-arg receiver family (`methods_narg/dispatch_1arg.rs`) | `[[2,3],[4,[5,6]]]».pairup` **aborted** `roast/S03-metaops/hyper.t` mid-file with "Odd number of elements"; `».pick(*)` shuffled the wrong level | slice 1 moved the *0-arg* `.pick`/`.roll`/`.head`/`.tail` forms onto `value_to_list_for_receiver`; the n-arg forms (`.head(n)`, `.tail(n)`, `.combinations`, `.batch`, `.fmt`, `.pick(n)`, `.roll(n)`) were the other half of the same set, and all eight `value_to_list(target)` sites in that file moved together. `&combinations(Iterable, k)` needed it too — its hand-rolled itemized-`Array` unwrap covered the kind but not the Hash flag. |
| `.trans` grouped operands (`value_to_string_list`) | `'a'..'z' => ['n'..'z','a'..'m']` stringified each whole `Range` instead of expanding it (`roast/S05-transliteration/trans.t`) | the grouping means "expand each of these in turn", so the element's itemization is stripped before deciding how to expand. |

**And one desugar the slice made visibly wrong.** `my (@a, @b) := (@x, @y)` desugars to a staging
`my @__destructure_tmp__ = <rhs>.list` plus one `my @a = @__destructure_tmp__[0]` per target
(§1.7 already flagged this desugar as approximate). Two things had to change:

1. **The staging temp is exempted from element itemization**
   (`Interpreter::itemize_elements_for_var_assign` / `is_destructure_staging_temp`,
   `src/vm/vm_var_assign_nil_decay.rs`). It is not a user `Array` — it *is* the RHS list, and
   every target reads a *value* out of it; raku stages that in a `Capture`/`List`, whose elements
   are not `Scalar`s. Itemizing it made a `%`-sigiled target read an itemized hash and die "Odd
   number of elements" (`roast/S06-signature/named-parameters.t` aborted mid-file). Two other
   passes already key on this same name for the same "compiler artifact, not a user container"
   reason (`parser/sink_warn.rs`, `compiler/expr_block.rs`); retiring the name check means
   changing the desugar to stage a Capture, which is §1.7's own ticket. Staging it as a genuine
   *bind* was tried first and is worse: `my ($b) = ()` then reads `Nil` instead of `Any`, and
   `my ($b) = 5` fails the `@`-sigil Positional bind check.
2. **A non-slurpy `@`/`%` target in binding mode is a genuine bind** (`Stmt::MarkBind` + the
   declaration). Measured: `my @x = 1, 2; my (@a,) := (@x,); @a.push(3)` writes through to `@x`
   in raku, so `@a` must be the element itself, not a copy. A slurpy `*@rest` is excluded — its
   read is a *slice* of the temp, and raku gives `@rest` an `Array` there
   (`roast/S02-names-vars/signature.t`).

`=`-mode keeps its greedy-slurp semantics unchanged.

**Known remaining divergences of the same "attribute read decontainerizes" family, deliberately
not chased here** (all pre-existing — they already reproduced on slice-1-itemized elements, and
none is covered by §1.3's matrix): `@c.Bag.raku` renders `($[1, 2]=>1).Bag` where raku renders
`([1, 2]=>1).Bag` (raku's `Set` is the *other* way — `Set.new($[1, 2])` in both), and the lazy
`IndexTransform::AntiPairs` pull path (`vm_helpers_lazy_pull.rs`) has no de-itemization. The
principled statement is "a `Pair`'s key is not a container"; making that a property of `Pair`
construction rather than of individual producers belongs with Slice 3, which is where the
container-vs-value distinction gets stated once in code.

**Perf (§5.2): the open question resolves in the fix's favour, and the mitigation is the model
itself.** The `ValueView::Array` arm's existing "share the `Gc` unless a `ContainerRef` is
present" optimization is untouched — `itemize_real_array_elements` runs *after* it and adds a
second, independent scan-then-rebuild-only-if-hit pass, so `my @a = @b` over a flat array of
scalars still costs a refcount bump and nothing else. The nested case (`@b` an array of arrays)
does not degenerate either, because slice 2 makes itemization **idempotent along a copy chain**:
`@b`'s elements were already itemized when `@b` was built, so the scan finds nothing and shares
the `Gc` too. The rebuild is therefore paid *at most once per aggregate*, at the moment it first
enters a real container — which is precisely the array-literal / list-assign site where §5.2
noted the per-element `match` is already being paid. No local A/B numbers are recorded here on
purpose (CLAUDE.md: document-grade numbers come from the bench CI); slice 5 checks the series.

**§5 Q4 ("does anything depend on an element being bare?") answers no.** 30 probes over
`to-json`/`from-json`, `is-deeply` (three shapes), `eqv`, `.WHICH`, `.Str`/`~`/`.gist`/`.join`,
`for`-with-`@row`, `.^name`/`~~ Positional`/`~~ Array`, `@`- and `Array`-typed parameter binding,
hyper `>>.`, `map -> @r`, `sort(*.[0])`, nested element assign, and the three autovivifying-push
shapes all agree with raku, and so do the 30 receiver-decomposition probes. The one place the ADR
predicted a stray `$` could surface — a serializer — turned out to need a fix in the *opposite*
direction (JSON was missing the itemization, not leaking it).

**Verification**: `cargo fmt --check` and `cargo clippy -- -D warnings` clean. Full local `t/`
suite passes (3490 files, 34k assertions). `t/element-store-itemization.t` grew from 46 to 100
assertions, every new one dual-oracled against `raku`: non-declaring `@a = …` / `%h = …` assign,
array and `%(…)` literals, the `(...)`-List-literal invariant, `.Array` / `.List` / `.list` /
`List.List`, the `my @b = @a` copy no-op (both `@b[0].raku` itemized and `@b.raku` bare), each
aggregate kind §2 names as a stored element (`Seq`, `Range`, `Hash`), flat-list hash
construction and `.Hash` coercion, a reified `gather`, the `:=`-bind invariant, three arity
invariants, native-array safety, a dedicated section pinning the counter-currents (including the
`[Z]`-vs-explicit-`Z` asymmetry and the `[+] @m[0]` one-arg rule), and the JSON round-trip. The
**full** `make roast` suite (1436 files, 218 836 tests) passes on a release build — not just a
targeted batch; see the distribution note above for why that distinction mattered here.

**Closed by this slice**: `todo/tickets/array-literal-nested-element-itemization-lost-in-raku.md`
(`say .raku for [3,2,[1,0]]` now prints `$[1, 0]`), retired to
`news/2026-08/array-literal-nested-element-itemization.md`.

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

### Slice 3 (2026-09-01) — the reflection side

Row 24 is the same model seen through `.VAR`, and it is the one place the *value* cannot carry the
answer. Slices 1-2 put the property on the stored value, so an element that is an aggregate already
reflected correctly with no reflection-specific work (`[1,(2,3)][1].VAR.^name` was already `Scalar`,
because slice 2 itemizes an array literal's elements, and `(1,(2,3))[1].VAR.^name` was already
`List`). But a bare `Int` element has no flag to carry: `my @c = 1, …` and `my @l := 1, …` hold a
byte-identical first element and must answer `Scalar` and `Int` respectively. The answer therefore
has to be read off the **source container's kind** — exactly the discriminator §1.6 established, now
stated once in code.

**`Value::elements_are_containers`** (`src/value/value_methods_a.rs`), beside slice 1's
`itemize_for_element_store` and slice 2's `needs_element_itemization`/`deitemize_element`:
`Array`/`Shaped`/`Lazy`/`ItemArray` and every `Hash` answer `true`; `List`/`ItemList`, `Seq`,
`Range` and everything else answer `false`; a `Scalar` wrapper recurses into what it holds, so a
`List` living in a `$` (`my $sl = (1,(1,2),[3,4]); $sl[1].VAR.^name` is `List`) answers from the
List. `Shaped` and `Lazy` are included even though `ArrayKind::itemize()` is a no-op on them — they
are real `Array`s, and raku agrees (`my @a = ^Inf; @a[1].VAR.^name` is `Scalar`).

**The consumer is `builtin_index_var_meta`** (`src/runtime/builtins.rs`), the runtime half of the
compiler's `.VAR`-on-a-subscript rewrite (`compile_expr_method_var_on_index`,
`src/compiler/expr_method.rs`). It used to synthesize an opaque `Scalar` descriptor
*unconditionally*, the one pre-existing exception being `Map` — whose values are famously not
containers, which is the same rule this slice generalizes.

**The compiler hook was rewired rather than the builtin taught to index.** The old shape compiled
the subscript's *target* purely for side effects, `Pop`ped it, and passed `(name, index)` so the
builtin could re-derive a `Map` value by key. Re-deriving the element inside a reflection builtin
would have been a hand-rolled duplicate of the subscript machinery — the compensator-per-site shape
this ADR exists to avoid, and one that would have had to grow arms for negative indices, `Seq`
reification, `Range` arithmetic and so on. The hook now compiles the **whole subscript** and passes
`(element, name)`, so the element is read once by the ordinary machinery and the builtin's only job
is to decide which of the two things to hand back. That also collapses the `Map` special case into
the general rule and made the `Seq`- and `Range`-sourced rows fall out for free
(`my $sq = (…).Seq; $sq[2].VAR.^name`, `my @r := 1..3; @r[1].VAR.^name`), where the hand-rolled
version answered only `List`.

**One representation ambiguity had to be resolved by the sigil.** mutsu's `ValueView::LazyList` is
the reified form of *both* a real `Array` assigned a lazy source (`my @a = ^Inf`, `my @a = lazy
gather {…}` — raku reports `Scalar` elements) and a lazy `Seq` (`my $s = lazy gather {…}` — raku
reports the values), so the value alone cannot answer. The variable's sigil resolves it, and does
so soundly rather than heuristically: raku **rejects** binding a `Seq` to an `@` variable outright
("Type check failed in binding; expected Positional but got Seq"), so an `@`-sigiled `LazyList` can
only have got there by assignment, i.e. it is a real `Array`. That, plus the `Map` declared-type
check, is why the two container-level distinctions live in an `Interpreter` wrapper
(`container_elements_are_containers`) and not in the `Value` method — both need context a `Value`
does not carry. `@`-assigned `Seq` and `Range` sources need no such rule: mutsu already reifies
those into a real `Array` at the store (slice 2), so their kind is faithful.

**`.VAR.name` was wrong in the same function and is fixed with it.** Rakudo names an element's
container after the container it lives in — `@real[0].VAR.name` is `@real`, `%h<a>.VAR.name` is
`%h` — where mutsu synthesized `@real[]` / `%h[]`. Nothing keyed off the suffix (a `.VAR` reflection
object is identified by its `__mutsu_var_target` attribute, not by its `name`), and no `t/` or roast
test pinned it.

**No counter-currents.** Unlike slices 1 and 2 — where the recurring trap was a reader asking a
question *about the value* while holding something itemized *because it is an element*, 17 such
sites in slice 2 alone — this slice changes no stored value and no flattening decision. It changes
what one reflection builtin returns, and the extra element read the rewired hook performs replaces
a read the subscript would have done anyway in every other context.

**Known remaining `.VAR` divergences, deliberately not chased here**, all of one family: mutsu's
`.VAR` on a *real* element returns an opaque descriptor carrying `name`/`dynamic`/`default`, while
raku returns the element's actual container, which delegates value methods through
(`@real[1].VAR.raku` is `[3, 4]` / `Scalar.new`, `.VAR.elems` `2` / `1`, `.VAR.gist` `$[3, 4]` /
`Scalar.new`). Fixing that means `.VAR` returning ADR-0036's `ContainerRef` element cell instead of
a descriptor — the *aliasing* surface, and a representation decision of its own. Recorded with its
three smaller siblings (the `@a[i;j]` multi-dim subscript never reaching this path at all, `is
default(0)` not reflected in `.VAR.default`, and native `int @a` elements needing an `IntPosRef`
mutsu has no representation for) as
`todo/deep/var-on-a-real-element-is-an-opaque-descriptor-not-the-container.md`.

**Verification**: `cargo fmt --check` and `cargo clippy -- -D warnings` clean.
`t/element-store-itemization.t` grew from 121 to 149 assertions and is `todo`-free for the first
time: row 24 un-marked, plus a dedicated slice-3 section dual-oracled against `raku` covering the
three real-Array-element shapes, seven non-container sources (`:=`-bound List, a List in a `$`, a
sigilless alias, a cached `Seq`, a raw `Seq` in a `$`, a bound `Range`, a `Range` in a `$`, a lazy
`gather` in a `$`), the real-`Array` kinds whose lazy or reified source could have been mistaken for
one of those (`^Inf`, an `@`-assigned lazy `gather`/`Seq`/`Range`, an unfilled element), `Hash` vs
`Map`, the two unnamed-subscript invariants, and the `.VAR.name` correction. Full local `t/` suite
passes; `make roast` delegated to CI.

### Slice 4 (2026-09-01) — the chained-subscript store, and what the compensators actually cover

Slice 4 was scoped as "delete the compensators, once slices 1-2 make them redundant". **They were
not redundant, and the reason was a bug, not a leftover.** Instrumenting both compensator sites
behind an env var and running the whole corpus is what found it — §1.3's divergence matrix could
not, because the matrix only exercises one subscript level.

**The instrumentation had to be split before it said anything true.** A first pass counted 57
firings of `raku_hash_value` across `t/` and looked like proof the render compensator was load
bearing. It was not: `raku_hash_value` has two callers with opposite roles — the three Hash/Map
rendering sites (the actual compensator) and the `ValueView::Scalar` arm
(`raku_repr.rs`), which is the **primary** `$(…)` renderer for every itemized value and not a
compensator at all. Probing only the three Hash/Map sites dropped the count to 22, and every one of
those was a real defect.

#### The defect: a chained subscript stores bare

Slice 1's own implementation note said "deeper (3+-level) chained assignment
(`exec_index_assign_deep_nested_op`/`exec_index_assign_generic_op`) was not separately audited". It
was not covered — and neither was the **leaf of a two-level chain**, nor the intermediate a
**deferred vivification token** walk-creates. Measured against raku:

| program | raku | mutsu (before) |
| --- | --- | --- |
| `my %h; %h<a><b> = [1,2]; %h<a><b>.raku` | `$[1, 2]` | `[1, 2]` |
| `takes(%h<a><b>)` | `1` | `2` |
| `my %d; %d<a><b>[2] = "z"; %d<a><b>.raku` | `$[Any, Any, "z"]` | `[Any, Any, "z"]` |
| `my @g; @g[0][1][2] = 7; takes(@g[0][1])` | `1` | `3` |
| `my %h; %h<a>[0]<k> = 5; %h<a>.raku` | `$[{:k(5)},]` | `[{:k(5)},]` |
| `my %h; my $r := %h<a>[1]; $r = "x"; %h<a>.raku` | `$[Any, "x"]` | `[Any, "x"]` |

The render-side compensator made `%h.raku` right while `%h<a><b>.raku`, `.VAR` and list-context
arity were all wrong — §1.5's "one value, three answers" shape, alive one level down and invisible
to the ADR's own oracle.

#### The fix — four sites, all "a value entering a parent's element slot"

- **`exec_index_assign_expr_nested_op`** (two-level): the leaf value, hooked once after the
  junction/slice arm, mirroring slice 1's hook at the single-level op's entry.
- **`exec_index_assign_deep_nested_op`** (3+): the same hook for the leaf, plus
  `Interpreter::fresh_autoviv_container` for the four intermediate-vivification sites (two array
  arms, two hash arms, first attempt and retry). Itemizing an `Array` only flips its `ArrayKind`
  tag, so the `&mut` the walk takes into the slot to keep descending is unaffected.
- **`fresh_level_for`** (`src/value/entry_path.rs`): the same for the container a deferred
  vivification token walk-creates — a different mechanism reaching the same slot.
- **An itemized-`Hash` arm for `.VAR`** (`methods_call_dispatch.rs`). An itemized `Hash` carries
  its itemization as a bool on the repr rather than as an `ArrayKind`, so the existing
  itemized-`Array` arm did not cover it and `%g<a>[0].VAR.^name` answered `Hash`. Only `.VAR` is
  redirected — unlike the `Array` case there is nothing to decontainerize for other methods.

`:=` through a **two-level** chain now matches raku too (`%h<a><b> := @s` renders `$[1, 2]`, `@s`
read directly stays bare, and a later `@s.push` still shows through). A **3+-level** bind installs a
shared `ContainerRef` cell rather than a value, and wrapping *that* in a `Scalar` — slice 1's
`@a.push(@b)` shape — **breaks the write path**, which does not yet see through a `Scalar`-wrapped
cell (`t/deep-element-bind-writeback-coherence.t` and `t/element-bind-cell.t` caught it). Left bare
deliberately; the write-through is pinned instead.

#### The compensators: measured, not deleted

Both sites were instrumented and the **entire** corpus run — `t/` (3601 files) and the full roast
whitelist (1425 files):

| compensator | before slice 4 | after slice 4 | what still reaches it |
| --- | --- | --- | --- |
| render-side (`raku_hash_value` at the 3 Hash/Map sites) | 22 in `t/` | **0 in `t/`**, 1 in roast | a self-referential hash (`:__mutsu_self_hash_ref`), where the value rendered is the cycle sentinel rather than the stored container |
| read-side (`itemize_hash_value`) | 3 in `t/` | 3 in `t/`, 17 in roast | **natively constructed hashes only** — Pod block `.config` (12 of the 17, `roast/S26-documentation/09-configuration.t`), `gethost(…)<addrs>`, and two exception/`Proc` hashes |

So slice 4's deletion is blocked on one nameable class, and it is the same class slice 2 already hit
once: **a `Hash` a native Rust builtin constructs directly bypasses every store hook**. Slice 2
fixed the JSON decoder by hand for exactly this reason; Pod `.config`, `gethost` and their siblings
are the rest of it. Deleting either compensator before those are routed through the itemizing store
would turn 20 measured firings into 20 wrong answers. That is the remaining slice-4 work, and it is
a store-site enumeration, not a mechanism question.

**Verification**: `cargo fmt --check` and `cargo clippy -- -D warnings` clean. Full local `t/`
suite (3601 files, 36214 tests) passes. `t/element-store-itemization.t` grew from 149 to 175
assertions and matches `raku` line for line: the chained-leaf shapes (two- and three-level, hash,
array, and both mixed orders), the autovivified intermediates, the deferred walk-create shapes, the
two-level bind's itemization *and* write-through, the 3+-level bind's write-through, and the
invariants (`%h.raku` / `@a.raku` unchanged, chain arity unchanged, the assignment expression's own
value itemized). The full roast whitelist (1425 files) passes on a release build.

### Slice 4b (2026-09-02) — the constructor is the store, and both compensators are deleted

Slice 4 ended with the deletion blocked on one nameable class: a `Hash` a native Rust builtin builds
directly bypassed every store hook. Slice 4b closes it, and the compensators are gone.

#### The store is `Value::hash`, not 160 call sites

The ticket proposed enumerating the native construction sites. There are ~160 `Value::hash(...)`
call sites, so the enumeration went one level down instead: **`Value::hash` itself** is the single
funnel all of them pass through, and it is precisely what ADR-0040 calls the store. The hook is the
scan-then-rebuild-only-if-needed shape `itemize_real_array_elements` already uses for the array half
(§5.2), so a hash of plain scalars is never touched and the assignment paths — which itemize on
their own way in — pay only the scan.

#### The scan had to stop going through `view()`

The first build broke `value::match_lazy::tests::lazy_match_children_stay_lazy_one_level`.
`Value::needs_element_itemization` decided through `Value::view()`, and a `view()` of a lazy `Match`
**forces** it (ADR-0016 P5) — so scanning a hash of capture nodes materialized every one of them
just to conclude that a `Match` never needs itemizing. It is a pure representation-tag probe now
(`NanBox::needs_element_itemization`): non-forcing, cheaper, and it removes the same latent forcing
from the array-half scan.

#### Four kinds of "hash" are not a `Hash`

The full `t/` suite against the central hook failed exactly four files, and every one named a real
distinction. mutsu represents several associative things with the `Value::Hash` repr whose values
raku says are **not** element containers:

| what | raku | mutsu before |
| --- | --- | --- |
| a `Map` (`Map.new(…)`, `.Map`) | `Map.new((a => (1,2)))<a>.raku` is `(1, 2)`, arity 2 | `$(1, 2)`, arity 1 |
| a `Match`'s capture map | `$/.hash<x>.VAR.^name` is `Array` | itemized, so `for $<hunk>` saw one item |
| a slurpy `*%h` parameter | `sub f(*%h){…}; f(a => ("x","y"))` sees `("x", "y")`, arity 2 | `$("x", "y")`, arity 1 |
| `%_` / leftover-named | the same hash under another name | idem |

They build through a second constructor, `Value::hash_bare_values` — the old `Value::hash` body.
The split is by *what kind of associative thing this is*, not by call site: a plain `%`-param that
receives a real `Hash` still sees that hash's own itemization
(`sub f(%h){…}; f({a => ("x","y")})` is `$("x", "y")`).

#### The measurement that justified the deletion

Both compensator sites instrumented behind `MUTSU_COMP_PROBE` (the render side probing ONLY the
three Hash/Map callers — see slice 4's trap), whole corpus, `t/` (3601 files) + full roast whitelist
(1435 files), with the store fix in place:

| compensator | slice 4 | slice 4b | what still reaches it |
| --- | --- | --- | --- |
| read-side (`itemize_hash_value`) | 3 in `t/`, 17 in roast | 7 in `t/`, **0 in roast** | only bare-valued hashes — 6 of the 7 are slice 4b's own new counter-current pins (`Map`, `.Map`, slurpy), the 7th is a `Capture`'s `.hash` |
| render-side (`raku_hash_value`, 3 Hash/Map sites) | 0 in `t/`, 1 in roast | 0 in `t/`, 1 in roast | the same self-referential hash, where the rendered value is the cycle sentinel and the test only asserts `ok $foo.raku` |

The 17 roast firings — the whole native-hash class — are gone. What is left is not a gap the
compensator covers but the compensator **producing the wrong answer**: every one of the seven is a
bare-valued hash the compensator re-itemizes. So deleting is a fix, not merely safe.

Both are deleted. `raku_hash_value` disappears entirely; the `ValueView::Scalar` arm — its other,
non-compensator caller — inlines `itemize_scalar_repr(inner, raku_value(inner))`, which is what it
always meant.

#### `.Map` had to grow the decont it only half had

`to_map` deconted its values only when the receiver was already a `Hash`. A
list-of-`Pair`s receiver folded through `to_hash` — which now itemizes, because that is what a
`Hash` store does — and kept the itemization, so `C.new(|(a => (1,2,3), b => (4,5,6)).Map)` bound
`Int @.a` to one `List` instead of three `Int`s (`roast/S32-hash/map.t`, "Map does not introduce
bogus Scalar containers"). Before this slice that path stored bare *by accident* and the render
compensator made `.raku` look itemized anyway — the same "one value, three answers" shape, in the
other direction. Both `.Map` arms decont now, in place, so the coercion keeps whatever metadata it
attached (a Set/Bag origin's `original_keys`).

#### The one surface that did not follow

`.VAR` on a bare-valued hash still answers `Scalar` rather than raku's `List`.
`Value::elements_are_containers` (slice 3's one-place discriminator) answers `ValueView::Hash(_) =>
true` unconditionally, and unlike an array — where `ArrayKind` carries the distinction — a `Hash`
has nowhere to read the bit from. This is pre-existing (before slice 4b the read compensator made
`.raku` and arity wrong in the same place), and it is filed as
`todo/tickets/var-on-a-bare-valued-hash-answers-scalar.md`: give `HashData` the missing bit.

**Verification**: `cargo fmt --check` and `cargo clippy -- -D warnings` clean; unit tests pass; the
full local `t/` suite passes. `t/element-store-itemization.t` grew a slice-4b section dual-oracled
against `raku` — a natively built `Hash` (`.classify`, *bound*, so no Raku assignment store can
paper over the construction) agreeing across the subscript read and `.values`, plus the four
counter-current shapes. The full roast whitelist (1435 files, 218833 tests) and the bundled-battery
gate both pass on a release build.

---

---

*If the mechanism judgment changes later, supersede this ADR rather than rewriting it.*
