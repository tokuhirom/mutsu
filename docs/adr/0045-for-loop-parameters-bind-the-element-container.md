# ADR-0045: A `for` loop parameter binds the element *container*; the per-iteration writeback is retired

- **Status**: Proposed (design complete; implementation not started)
- **Date**: 2026-08-20
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md) §7 (which names
  this finding as "the natural *next* consumer of this ADR's routing" and supplies the primitive and
  the replace-vs-alias answer), [ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md) §7
  (the representation surface of the same Raku model; its §1.2 bind-side itemization is the code this
  ADR's bind site sits next to), [ADR-0013](0013-container-interior-mutability-cellvalue.md) §5 Q3 /
  §7 (the "2c / Track B" deferral, released), [ADR-0027](0027-loop-frozen-value-capture-cascade.md)
  (per-iteration capture freezing — the invariant this ADR must not break),
  [ADR-0042](0042-type-constraints-belong-to-the-container-not-to-a-name.md) (the constraint half of
  §1.3 class 5), `todo/deep/for-loop-rw-element-alias-lost-through-deferred-closure.md` (the
  originating finding)

> Raku's `for @a -> $v is rw` binds `$v` **to the element's `Scalar` container**, for the lifetime of
> that binding. mutsu binds a plain value clone and copies it back into the array once, at the end of
> the iteration. This ADR decides to replace the copy-back with a real element-container bind, using
> the primitive ADR-0036 shipped, and records that the originating finding's stated blocker no longer
> exists.

---

## 1. Context

### 1.1 What Raku specifies

`for` binds its parameter to the *item* the iterator yields. When the source is a real mutable
`Array`/`Hash`, that item is the element's `Scalar` container, so the binding is an alias with the
lifetime of the binding, not of the loop body:

```
$ raku -e 'my @a = <A B>; for @a -> $v is rw { $v = "x" }; say @a'      # [x B]
$ raku -e 'my @a = 10,20; my @c; for @a -> $v is rw { @c.push(-> { $v = $v + 1 }) };
           @c[0](); @c[1](); say @a'                                     # [11 21]
```

Three consequences follow, and all three are observable:

- **A closure that escapes the loop still writes through.** There is no "end of iteration" event that
  ends the alias.
- **The alias and the array are the same storage, in both directions.** A body that writes `@a[1]`
  directly and a body that writes `$v` write to the same place, in program order, with no
  last-writer-wins step appended afterwards — and a *read* of `$v` after a direct write to the
  element sees the new value (`for @a -> $v is rw { @a[0] = 9; say $v }` prints `9`).
- **The alias is the *item the iterator yielded*, not "index `idx` of the source".** `for @a.reverse
  -> $v is rw` and `for @a.sort -> $v is rw` alias the elements in the derived order, because
  `.reverse`/`.sort`/`.values` over an `Array` hand out its element containers.

**Which parameter forms alias — measured, not assumed.** `is rw`, `<->`, sigilless `\v` and the
**implicit topic `$_`** all alias in both directions; the plain named parameter `-> $v` does **not**
— it is a read-only binding of the value, and raku prints `1` for
`for @a -> $v { @a[0] = 9; say $v }` and `1 2` for the deferred-closure read. This asymmetry is the
single most important input to §4's phasing, because it means the plain-named-param half of the
writeback has **nothing to replace it with** and is a pure deletion.

Where the source is *not* a mutable container — a `List`, `Seq`, or `Range` — the item is immutable
and `is rw` binding fails at bind time:

```
$ raku -e 'for (1,2) -> $v is rw { $v = 5 }'
Parameter '$v' expects a writable container (variable) as an argument,
but got '1' (Int) as a value without a container.
```

### 1.2 What mutsu does

`for` binds a **plain value clone** and then **copies it back once per iteration**:

- **The bind** (`src/vm/vm_for_loop_body.rs:521-538`): the item — after ADR-0040's bind-side
  itemization at `:521-526`, which is skipped when `spec.do_writeback` — is inserted into `env` and
  the param's local slot by value. Nothing links it to the source element.
- **The copy-back**, an entire family of ~700 lines:

  | function | file:line | role |
  | --- | --- | --- |
  | `write_back_for_rw_param` | `vm/vm_loop_writeback_quant.rs:261` | the `is rw` / `<->` / `\v` sibling |
  | `write_back_for_topic_item` | `vm/vm_loop_writeback.rs:278` | the implicit-topic and plain-named-param sibling |
  | `write_back_hash_value_item` | `vm/vm_loop_writeback_quant.rs:183` | `%h.values` by pre-captured key order |
  | `write_back_to_source_var` | `vm/vm_loop_writeback_quant.rs:228` | the scalar-source sibling |
  | `write_back_container_source` | `vm/vm_loop_writeback.rs:252` | the shared store (env + local slot + `ContainerRef` write-through) |
  | `write_back_quanthash_rw` / `write_back_quanthash_value_item` | `vm/vm_loop_writeback_quant.rs:136` / `:109` | QuantHash **weights** — a genuinely different operation, see §2.4 |

  They are wired in from **five duplicated call-site pairs** in `vm_for_loop_body.rs` — the `Ok`,
  `is_succeed`, `next`, `last`, and residual arms at `:647`, `:699`, `:751`, `:797`, `:833`.

Each call **rebuilds the whole backing `ArrayData`** with one element replaced
(`vm_loop_writeback.rs:435-437`, `vm_loop_writeback_quant.rs:399-405`) and stores the new container
back over the source variable. `loop_var_unchanged` (`vm_loop_writeback.rs:146`) exists solely to
skip that rebuild when the loop variable is provably unmodified, "otherwise even a read-only loop
[…] would be O(n^2)" — its own comment. A *mutating* loop gets no such escape (§1.5).

So the alias in mutsu is not an alias at all: it is a **snapshot taken at one instant, at the end of
the body**, plus a wholesale container replacement. Every divergence below follows from those two
facts.

### 1.3 The divergence, measured on `main` (33f75a62f, 2026-08-20)

Each row is its own block so no earlier statement contaminates it (`tmp/rwalias/table*.raku`).

| # | program (abbrev.) | raku | mutsu |
| --- | --- | --- | --- |
| 01 | `for @a -> $v is rw { @c.push(-> { $v = $v+1 }) }`, called after | `[11 21]` | `[10 20]` |
| 02 | same with `<-> $v` | `[11 21]` | `[10 20]` |
| 03 | same with `-> \v` | `[11 21]` | `[10 20]` |
| 04 | `for @a -> $v is rw { $v = $v+1; @a[1] = 99 }` | `[11 99]` | `[11 21]` |
| 07 | `try { for @a -> $v is rw { $v = 77; die } }` | `[77 20]` | `[10 20]` |
| 08 | `for %h.values -> $v is rw { @c.push(…) }`, called after | `{a => 2}` | `{a => 1}` |
| 11 | deferred closure *reads* `$v` after `@a[0] = 5` | `5` | `10` |
| 12 | two closures pushed in the **same** iteration, both `$v = $v+1` | `[12]` | `[10]` |
| 13 | closure called **inside** the loop *and* after | `[12 22]` | `[11 21]` |
| 14 | `-> $v is rw { @c.push(-> { $v = [9] }) }` over an array of arrays | `[[9] [3 4]]` | `[[1 2] [3 4]]` |
| 16 | `for @a.kv -> $i, $v is rw { @c.push(…) }`, called after | `[11 21]` | `[10 20]` |
| 17 | `for @a.reverse -> $v is rw { $v = $v+1 }` | `[11 21]` | **`[21 11]`** |
| 19 | `for (1,2) -> $v is rw { $v = 5 }` | dies | silent no-op |
| 20 | deferred closures read after `@a[0]=5; @a[1]=6` | `(5 6)` | `(10 20)` |
| 21 | `for @a { $_ = $_+1; @a[1] = 99 }` | `[11 99]` | `[11 21]` |
| 22 | `for @a -> $v { @a[1] = 99 }` — **no `rw` anywhere** | `[10 99]` | `[10 20]` |
| 24 | `for @a.sort -> $v is rw { $v = $v+1 }` | `[21 11]` | `[20 10]` |
| 27 | `for @a -> $v is rw { @p.push(start { $v = $v+1 }) }; await @p` | `[11 21]` | `[10 20]` |
| 28 | `my Int @a; for @a -> $v is rw { $v = "s" }` | dies | succeeds |
| 30 | `for 1..2 -> $v is rw { $v = 5 }` | dies | silent no-op |
| 36 | one closure writes `$v`, a sibling closure in the same iteration reads it | `9` | `1` |
| 38 | `for @a -> $v is rw { $v = 9; @a = 7,8 }` | `[7 8]` | **`[7 9]`** |
| 39 | `for @$s <-> $x { $x = $x+1 }` | `[2 3]` | `[1 2]` |
| 41 | `for @a -> $v is rw { @a[0] = 9; say $v; last }` | `9` | `1` |
| 42 | `for @a { @a[0] = 9; say $_; last }` — implicit topic | `9` | `1` |
| 43 | `for @a -> \v { @a[0] = 9; say v; last }` | `9` | `1` |
| 44 | `for @a { @c.push(-> { $_ = 99 }) }; @c[0]()` | `[99 2]` | `[1 2]` |

**Invariants that already agree and must not move** (same measurement run):

| # | program | both |
| --- | --- | --- |
| 05/06 | `last` / `next` after mutating `$v` | `[77 20]` / `[77 77]` |
| 09 | `for %h.values -> $v is rw { $v *= 10 }` | `(a => 10 b => 20)` |
| 10/32/33 | plain named param mutating a container element in place (`$row.push(9)`) | propagates |
| 15 | `for @a.kv -> $i, $v is rw { $v += $i }` | `[10 21]` |
| 23/26 | `.values` / shaped-array `is rw`, direct | `[11 21]` |
| 25 | body mutates and `last`s partway | `[11 21]` |
| 29 | `for @a.map({$_}) -> $v is rw` (Seq source) | no die |
| 31 | `for @a -> $v { $v = 5 }` — read-only param | dies |
| 34/35 | ADR-0027 per-iteration capture identity | `(1 2 3)` |
| 37 | nested `for @m -> @row { for @row <-> $x { … } }` | `[[10 20] [30 40]]` |
| 40 | `@a.raku` / `.elems` after an `is rw` loop — the cell stays invisible | `[2, 3]` / `2` |
| 45 | `for @a -> $v { @a[0] = 9; say $v }` — plain param must **not** read-alias | `1` |
| 46 | `for @a -> $v { @c.push(-> {$v}) }; @a[0]=9` — nor deferred | `1 2` |

Read the failure column as **five classes**, only the first of which the originating finding
described:

1. **Lost writes through a binding that outlives the body** — rows 01, 02, 03, 08, 12, 13, 14, 16,
   27, 36, 44. The snapshot is taken before the closure (or `start` block) ever runs.
2. **Stale reads through an aliasing binding** — rows 11, 20, 41, 42, 43. The binding never sees a
   write made to the element by anyone else, whether that write happens later in the same body
   (41-43) or after the loop (11, 20). No widening of the writeback can fix these: it runs at
   *iteration end* and there is no later event to hook.
3. **The snapshot clobbers the body's own direct writes** — rows 04, 21, 22, 38. The whole-container
   rebuild is a last-writer-wins store of a container captured *before* the body ran, so
   `@a[1] = 99` inside the body is silently reverted. **Row 22 needs no `rw` and no closure**: a
   plain `for @a -> $v { @a[1] = 99 }` loses the write. Row 38 is the sharpest form — the body
   rebinds `@a` wholesale and the writeback then writes the old iteration's value into the *new*
   array.
4. **Index mis-mapping for derived sources** — rows 17, 24, 39. `write_back_for_rw_param` is not
   passed `container_reversed` at all (compare `vm_for_loop_body.rs:654` for the topic sibling, which
   is), so `for @a.reverse -> $v is rw` writes each mutated value to the *mirror-image* index: `10,20`
   becomes `[21 11]` instead of `[11 21]`. `.sort` has no index mapping to reconstruct at all, so the
   writeback silently does nothing.
5. **Missing enforcement** — rows 19, 30, 28. A `List`/`Range` source accepts an `is rw` param and
   silently discards the write instead of dying at bind time; a typed array's element constraint is
   not checked. (The constraint half is ADR-0042's subject; ADR-0036 slice 4 owns the same gap for
   `:p` pairs.)

Classes 3 and 4 are the ones worth stressing when weighing this work: they are **silent data
corruption in ordinary, non-exotic loops**, with no closure, no `rw`, and no concurrency involved.

### 1.4 The originating finding's stated blocker no longer exists

`todo/deep/for-loop-rw-element-alias-lost-through-deferred-closure.md` concluded that a fix must wait
for a *share-vs-bind distinction at the element-store layer*, because the element store
"write-throughs **any** `ContainerRef` element unconditionally on reassign, with no way to
distinguish 'this element is a bind-target that should stay aliased' from 'this element was just
plain-reassigned and should replace whatever was there'."

**That distinction is not needed, and building it would be a bug.** ADR-0036 §7 answers it directly:
unconditional write-through *is* the Raku semantics — `@a[0] = "Q"` assigns *into* the element's
`Scalar`, it never replaces the container. Re-measured here, end to end, on `main`:

```
$ mutsu -e 'my @a = 1,2; my @c;
            for 0..1 -> $i { my $r := @a[$i]; @c.push(-> { $r = $r + 1 }) }
            @c[0](); @c[1](); say @a'                                        # [2 3]   (raku: [2 3])
$ mutsu -e 'my @b = 5; my @c;
            for 0..0 -> $i { my $r := @b[$i]; @c.push(-> { $r = $r + 100 }) }
            @c[0](); @c[0](); say @b'                                        # [205]   (raku: [205])
$ mutsu -e 'my @d = 1,2; my $s := @d[0]; @d[0] = 42; say $s'                 # 42      (raku: 42)
$ mutsu -e 'my %h = a => 1; my $hr := %h<a>;
            my $f = -> { $hr = $hr + 1 }; $f(); say %h'                      # {a => 2}(raku: {a => 2})
$ mutsu -e 'my @c = 1,2; my $r := @c[0]; say @c.raku, " ", @c.elems'         # [1, 2] 2 (invisible)
```

The hand-written form of the proposed fix — a `ContainerRef` element bind captured by a closure that
escapes and is called later, twice, and read back through an unrelated path — **already produces
raku's answer on every probe**, including the write-invisibility invariant. The primitive
(`Value::array_slot_ref`, `src/value/value_methods_b.rs:94`; `Value::hash_slot_ref`,
`src/value/value_methods_a.rs:603`) shipped, is exercised daily by `:=`-bound elements, and ADR-0036
slice 2 already routed the subscript adverbs through it.

So the finding's own "Suggested next steps" §1 — "define the share-vs-bind distinction … before a
for-loop `is rw` param can bind a real alias" — is superseded. The work is **routing, not
invention**, and its §2 ("`write_back_for_rw_param` becomes largely unnecessary") is exactly right.

### 1.5 The writeback is also the reason a mutating `<->` loop is quadratic

Measured on a release build of `main` (33f75a62f), same machine, `for @a <-> $x { $x = $x + 1 }`:

| n | mutsu | raku |
| --- | --- | --- |
| 5 000 | 0.073 s | 0.003 s |
| 10 000 | 0.276 s | 0.001 s |
| 20 000 | 1.095 s | 0.001 s |
| 40 000 | 5.157 s | 0.012 s |

Doubling `n` multiplies the time by 3.8×, 4.0×, 4.7× — **O(n²)**, because every iteration clones the
entire `ArrayData` to change one element. The read-only sibling (`for @a <-> $x { $s = $s + $x }`,
where `loop_var_unchanged` fires) runs the 20 000-element case in **0.033 s**, 33× faster than the
mutating one. raku is flat.

This inverts the usual perf calculus for element-container promotion. ADR-0036 §5 Q1 and ADR-0040
§5.2 both had to weigh a *cost* for promoting elements. Here promotion **replaces an O(n) per-iteration
rebuild with an O(1) cell promotion**, so the fix removes a quadratic instead of adding a constant.
The promotion is also idempotent — `array_slot_ref` returns an existing cell rather than allocating a
second one (`value_methods_b.rs:115-117`) — so re-looping the same array is free after the first pass.

### 1.6 Why the index bookkeeping is the tell

`container_reversed`, `total_items`, `hash_keys_for_writeback`, `values_mode`, `kv_mode`,
`container_source_slot` and `spec.rw_param_names` exist in `vm_for_loop_body.rs` for exactly one
purpose: to **reconstruct, at iteration end, which slot of the source the item came from**. That
reconstruction is guesswork over a snapshot `Vec` (`chunked_items`, `:134-141`) that has already lost
the provenance — which is why it is wrong for `.reverse` (row 17), absent for `.sort` (row 24), and
silently skipped for `for @$s` (row 39).

An item that *is* its element's container needs no reconstruction: it carries its identity. So the
whole bookkeeping layer is not merely accompanying the defect, it **is** the defect, and it is
deletable rather than fixable.

---

## 2. Decision

**Bind a `for` loop parameter to the item the iterator yields, and — when the source is a real
mutable `Array`/`Hash` and the parameter is an aliasing one — make that item the element's
`ContainerRef` (`array_slot_ref` / `hash_slot_ref`). Retire the per-iteration writeback family for
element sources.**

Four parts:

1. **Where the alias is created.** At the bind site (`vm_for_loop_body.rs:521-538`), not at iteration
   end. When `container_binding` names a real mutable `Array`/`Hash` (the same discriminator ADR-0036
   §2.2 and ADR-0040 §1.6 arrived at independently: not `List`, `Seq`, `Range`, `Capture`, `Match`,
   or an immutable `Set`/`Bag`/`Mix`), the loop param binds `array_slot_ref(idx, true)` /
   `hash_slot_ref(key, true)`. Otherwise it keeps today's plain-value bind.

2. **Which parameters alias — and which explicitly must not.** The aliasing forms are `is rw`, `<->`,
   sigilless `\v`, and the **implicit topic `$_`** (rows 41-44). The plain named parameter `-> $v` is
   a read-only binding of the *value* and must stay one (rows 45-46 are agreeing invariants, not
   divergences). Row 22's plain-named-param divergence is therefore **not** a missing alias: it is
   purely the class-3 clobber, so its half of the writeback is deleted with nothing put in its place.

3. **Derived sources hand out element containers at the producer, not at the loop.** `.values`,
   `.kv`, `.reverse`, `.sort`, and `@$s` over a real `Array`/`Hash` must yield element containers,
   the same routing ADR-0036 slice 3 defines for `.pairs`/`.kv`/`.antipairs`. Then rows 17, 24 and 39
   fall out with no index mapping anywhere, and `container_reversed` / `total_items` /
   `hash_keys_for_writeback` are deleted rather than corrected.

4. **The writeback family is retired for element sources.** `write_back_for_rw_param`,
   `write_back_for_topic_item`, `write_back_hash_value_item`, and the ten call sites in
   `vm_for_loop_body.rs` go away. **`write_back_quanthash_rw` / `write_back_quanthash_value_item`
   stay**: a `BagHash`/`MixHash` weight is not a stored element container in mutsu's representation
   and `.value = 0` *removes* the key, so it is a different operation — the same carve-out ADR-0036
   §5 Q2 made, pinned by `t/for-pairs-value-quanthash-writeback.t`.

### Why this direction

- **It is the only direction that fixes classes 1-4 at once.** They have one cause (snapshot instead
  of alias) and one fix. No refinement of the writeback reaches class 2 at all — there is no later
  event to hook — and class 3 is *caused by* the writeback's existence, so making the writeback
  smarter cannot remove it.
- **It removes a mechanism instead of adding one.** ~700 lines of index reconstruction, five
  duplicated call-site pairs, and a special-case guard (`loop_var_unchanged`) whose only job is to
  make the mechanism affordable. Under the project's gain/risk definitions this is squarely a gain:
  the reconstruction is incomplete static reasoning over a snapshot that cannot be made sound.
- **It makes the fast case faster, not slower.** §1.5: a measured quadratic becomes linear, and the
  read-only case that `loop_var_unchanged` protects today becomes correct-by-construction rather than
  correct-by-guard.
- **The primitive is shipped and measured for this exact consumer** (§1.4), and ADR-0036 §7 already
  nominated this as its next consumer. This is routing, not a new campaign.
- **It answers the originating finding's blocker by deleting the question** (§1.4): unconditional
  element write-through is the requirement, not the obstacle.

---

## 3. Options considered

| Option | 1 lost writes | 2 stale reads | 3 clobber | 4 index | 5 enforce | perf | Verdict |
| --- | --- | --- | --- | --- | --- | --- | --- |
| **Status quo (snapshot + rebuild)** | ✗ | ✗ | ✗ | ✗ | ✗ | O(n²) | Rejected — the defect |
| **A. Pass `container_reversed` to `write_back_for_rw_param`** | ✗ | ✗ | ✗ | partial | ✗ | O(n²) | **Rejected as the answer, worth doing as a stopgap only if this ADR is deferred.** It is a genuine two-line bug fix for row 17, but it deepens the reconstruction layer this ADR deletes, and it cannot reach `.sort` (row 24), which has no index to map. |
| **B. Write back more often (after every statement / on closure call)** | partial | ✗ | ✗ (worse) | ✗ | ✗ | worse | Rejected. There is no bound on "often enough" — a closure can be called from another thread (row 27) or from arbitrarily deep. Each extra writeback is another whole-container rebuild, multiplying both the quadratic and the class-3 clobber. |
| **C. Keep the snapshot, but write back only the changed element via a targeted store** | partial | ✗ | partial | ✗ | ✗ | O(n) | Rejected. It cures the quadratic and the coarsest clobber but leaves classes 1, 2 and 4 untouched, and it keeps the index reconstruction — the part §1.6 identifies as unfixable. It is the shape that *looks* like progress while leaving the model wrong. |
| **D. Bind the element container; retire the writeback (this ADR)** | ✓ | ✓ | ✓ | ✓ | ✓ | O(n) | **Chosen** |
| **E. Full 2c: every element is a cell, always** | ✓ | ✓ | ✓ | ✓ | ✓ | ? | Deferred, not rejected — the same verdict as ADR-0036 option D and ADR-0040 option D. D promotes lazily, per element actually bound, which for a `for` loop is every iterated element anyway; so this ADR is in fact the largest natural probe of what E would cost, and its measurements should be treated as input to that decision. |

---

## 4. Phasing

Each slice is independently landable and independently green.

0. **Slice 0 — pin the semantics.** `t/for-loop-element-alias.t` covering every row of §1.3 as a
   currently-failing expectation set (`todo`-marked so it lands green), **plus the whole invariant
   table** — `last`/`next` (05/06), the direct-mutation rows (09, 15, 23, 26), in-place container
   mutation (10, 32, 33), the read-only-param die (31), ADR-0027's per-iteration identity (34, 35),
   nesting (37), and the cell-invisibility row (40). The invariant half is what stops later slices
   from "fixing" the divergence by over-promoting. Existing regression pins:
   `t/bound-array-for-rw.t`, `t/bound-hash-for-values-rw.t`, `t/for-loop-cell-elements.t`,
   `t/for-loop-named-param-alias.t`, `t/for-loop-instance-writeback.t`,
   `t/for-loop-param-start-sibling-isolation.t`, `t/for-loop-param-getupvalue-hijack.t`,
   `t/loop-var-closure-capture.t`, `t/loop-var-nested-closure-freeze.t`,
   `t/closure-rw-param-writeback.t`, `t/cas-shaped-and-for-loop.t`,
   `t/for-pairs-value-quanthash-writeback.t`, `t/proxy-list-transparency.t`,
   `t/for-bind-typed-array-deitemize.t`, `t/param-bind-itemization.t`. Several of these pass *only*
   via the writeback, so they are precisely what must keep passing without it.

   Add a bench probe for §1.5's mutating `<->` loop; it is the acceptance number for slice 1.

1. **Slice 1 — the direct array source, writable aliasing params.** `for @a -> $v is rw` / `<-> $v` /
   `-> \v` over a real `Array` binds `array_slot_ref(idx, true)`; `write_back_for_rw_param`'s array
   arm and its ten call sites go. Rows 01, 02, 03, 04, 07, 11, 12, 13, 14, 20, 27, 36 and 41 turn
   green and the quadratic disappears. This is first because it is the smallest closed set, it
   carries the whole perf win, and it exercises the ADR-0027 interaction (§5 Q1) against pinned rows
   34/35.

2. **Slice 2 — hash sources.** `for %h.values -> $v is rw` and the `%h`/`.kv` shapes via
   `hash_slot_ref`; `write_back_hash_value_item` and `hash_keys_for_writeback` go. Row 08 turns
   green.

3. **Slice 3 — the implicit topic and the plain named param.** These are two *different* jobs sharing
   one function (`write_back_for_topic_item`), and §1.1's measurement splits them cleanly:

   - **The topic promotes.** `for @a { … }` binds `$_` to `array_slot_ref(idx, true)`, so rows 42 and
     44 turn green along with row 21.
   - **The plain named param is a pure deletion.** `writes_back_named_param`
     (`vm_for_loop_body.rs:125-126`) has nothing to replace it with — rows 45/46 pin that `-> $v`
     must *not* alias, rows 10/32/33 already propagate in-place container mutation through the shared
     `Gc` with no writeback involved, and row 31 shows the param cannot be assigned at all. Deleting
     it is what turns row 22 green.

   Two places claim otherwise and must be probed first: `t/proxy-list-transparency.t` and the
   `for @$h` Text::CSV shape (`vm_loop_writeback.rs:306-318`), whose comment explicitly preserves a
   named-param no-writeback carve-out.

4. **Slice 4 — derived producers.** `.values` / `.reverse` / `.sort` / `.kv` / `@$s` over a real
   `Array`/`Hash` hand out element containers, sharing ADR-0036 slice 3's routing. Rows 16, 17, 24
   and 39 turn green; `container_reversed`, `total_items` and `write_back_to_source_var`'s element
   arm go. **Land this after or with ADR-0036 slice 3** — they are the same producer layer and
   splitting the routing would leave two half-converted method sets.

5. **Slice 5 — enforcement.** Reject an `is rw` bind against an immutable source at *bind* time
   (rows 19, 30) with raku's `Parameter '$v' expects a writable container (variable) as an argument,
   but got '1' (Int) as a value without a container`, and let the promoted
   cell carry the container's element constraint (row 28) — shared with ADR-0036 slice 4 and
   ADR-0042. Whichever of the three lands the constraint first owns it; the other two consume it.

6. **Slice 6 — sweep.** Re-run §1.3's tables and §1.5's bench, delegate the full `make roast` to CI,
   record the outcome in this ADR's "Implementation status", and `git mv` the originating finding to
   `news/2026-08/`.

---

## 5. Open questions (the forks for the deciders)

1. **Does an aliased loop param compose with ADR-0027's per-iteration freeze?** The freeze installs
   the closure's captured value over the caller's binding (`vm_closure_dispatch.rs:427-431`). If the
   captured value is a *cell*, re-installing it is exactly right — each iteration's closure holds its
   own iteration's cell, which is what rows 34/35 require and rows 12/36 require *within* an
   iteration. `compute_owned_captures` (`vm_register_ops.rs:459-494`) already refuses to cascade a
   `ContainerRef` through `frame_owned` for precisely this reason, but its **primary**
   `loop_local_vars` branch has no such guard. *Recommendation: slice 1 lands behind rows 34/35 and
   12/36 in the same test file — they pull in opposite directions and together they pin the
   behaviour. Check `box_captured_lexicals` does not double-box an already-cell param.*

2. ~~**Does the read-only plain param need to alias at all?**~~ **Answered by measurement (§1.1,
   rows 45-46): no.** `for @a -> $v { @a[0] = 9; say $v }` prints `1` in raku, and the deferred-read
   form prints `1 2` — a plain named parameter binds the value, not the container. So slice 3's
   named-param half is a pure deletion and this ADR does **not** promote an element for every
   ordinary `for @a -> $v` loop. The residual question is narrower: **the implicit topic does alias**
   (rows 42/44), and `for @a { … }` is the most common loop in the corpus, so the topic path is where
   per-element promotion cost, if any, will show up. *Recommendation: slice 3 measures the topic loop
   against the same bench probe slice 1 uses; `array_slot_ref`'s cell reuse makes a re-loop free, so
   the cost is one allocation per element per first pass.*

3. **What is the interaction with ADR-0040's bind-side itemization?** `vm_for_loop_body.rs:521-526`
   itemizes a plain `$`-param's value **except when `spec.do_writeback`** — i.e. the aliasing case is
   currently carved out. Under this ADR the param binds a cell wrapping the element, and the
   itemization question moves to what the cell *holds*, which is ADR-0040's store-side subject.
   *Recommendation: slice 1 keeps the existing carve-out and pins `t/param-bind-itemization.t` and
   `t/for-bind-typed-array-deitemize.t` unchanged; revisit only when ADR-0040 slice 2 lands.*

4. **Does anything read a for-loop's source array without decontainerizing?** More slots hold
   `ContainerRef` for longer than they do today, which is ADR-0036 §5 Q4's question at a larger scale
   — a `for` loop promotes every iterated element, where `@a[0]:p` promotes one. Row 40 pins the
   `.raku`/`.elems` invariant. *Recommendation: slice 1 lands behind slice 0; a leaked cell surfaces
   as a deterministic wrong `.raku`/`.WHAT`/`.gist`, not a flake.*

5. **Shaped and native-backed arrays.** Row 26 agrees today via the metadata-preserving rebuild
   (`vm_loop_writeback.rs:429-437`, which deliberately clones `ArrayData` rather than using
   `ArrayData::new` so `array[int]` typing and shape survive). `array_slot_ref` has no such
   provision, and a native-backed `array[int]` (ADR-0015) may not be able to hold a `ContainerRef` at
   all. *Recommendation: slice 1 keeps the writeback path for native-backed and shaped arrays as an
   explicit, commented carve-out, and slice 5 decides whether to remove it —`t/cas-shaped-and-for-loop.t`
   is the pin.*

6. **Does the `Proxy` element carve-out survive?** `write_back_for_topic_item` refuses to replace a
   `Proxy` element with its FETCHed value (`vm_loop_writeback.rs:348-353`,
   `t/proxy-list-transparency.t`). A cell bind never replaces anything, so the hazard should
   evaporate — but a `Proxy` mediates its own STORE, and binding a cell *around* one has not been
   probed. *Recommendation: one probe in slice 0.*

---

## 6. Consequences

- **`for` loop parameters become real aliases**, so mutation through a closure, a `start` block, or
  any binding that outlives the iteration writes through, and reads see later writes.
- **Three silent-corruption classes disappear** (§1.3 classes 3 and 4): a body that writes its source
  array directly, or rebinds it, or iterates it reversed/sorted, stops losing or misplacing writes.
  Row 22 in particular is a *plain* `for @a -> $v { @a[1] = 99 }`, which is ordinary code.
- **A measured O(n²) becomes O(n)** (§1.5): the 40 000-element mutating `<->` loop goes from 5.2 s to
  a linear pass. This should be visible in the bench CI series, and slice 1 should cite it.
- **~700 lines and five duplicated call-site pairs are deleted**, along with the index-reconstruction
  state (`container_reversed`, `total_items`, `hash_keys_for_writeback`) and the
  `loop_var_unchanged` guard that exists only to make the deleted mechanism affordable.
- **More array/hash slots hold `ContainerRef` during a loop.** This is the real blast radius, the
  same one ADR-0036 §6 names, at larger scale: every iterated element of a mutable source promotes.
  The failures it can produce are deterministic wrong renderings, not flakes.
- **New deterministic failures are expected in code that silently discarded writes** — an `is rw`
  loop over a `List`/`Range` starts dying (rows 19, 30) and a typed array starts rejecting bad
  elements (row 28). That is the fix working.
- **If rejected / indefinitely deferred:** the 27 divergences in §1.3 stay, including two forms of
  silent data corruption in loops that use no advanced feature at all; the mutating `<->` loop stays
  quadratic; and row 17's `.reverse` mis-mapping stays a two-line fix nobody will make because it
  sits inside a mechanism everyone knows is wrong. In that case **do at least take option A** as a
  stopgap and say so in the finding.

---

## 7. Adjacent findings — the same Raku model, three surfaces (now all designed)

"An `Array`/`Hash` element is a `Scalar` container" is one model with three consequences. As of
2026-08-20 all three have an ADR:

- **[ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md) — the representation surface.**
  An element read is one item and renders `$[…]`. Fixed by itemizing at the store.
- **[ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md) — the aliasing surface for
  pair producers.** `(@a[0]:p).value` is the element's container. Fixed by
  `array_slot_ref`/`hash_slot_ref` promotion at the producer; slices 1-2 landed.
- **This ADR — the binding-lifetime surface.** A `for` parameter is bound to the element container
  for the lifetime of the binding, not of the body. Fixed by the same promotion, at the loop's bind
  site, plus retiring the writeback.

The three are complementary, not competing, and they meet in two places worth naming: a promoted
cell must not de-itemize what it wraps (ADR-0040 §5.3), and the element type constraint belongs to
the container (ADR-0042), which all three consume in their enforcement slice.

---

*This ADR is Proposed. If the mechanism judgment changes later, supersede it rather than rewriting
it.*
