# ADR-0045: A `for` loop parameter binds the element *container*; the per-iteration writeback is retired

- **Status**: Accepted — partially implemented (slices 0-4 landed 2026-08-27, slice 5 landed
  2026-09-01; only row 16 (`.kv`) and slice 6's sweep remain, see §8 "Implementation status")
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
the provenance — which is why it was wrong for `.reverse` (row 17), absent for `.sort` (row 24), and
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

## 8. Implementation status

### Slices 0 and 1 — landed 2026-08-27

**Slice 0.** `t/for-loop-element-alias.t` pins the whole surface of §1.3: every divergence row and
every invariant row, plus §5 Q6's `Proxy`-element probe, §5 Q1's already-a-cell capture probe, and
§1.5's bench probe as an O(n) assertion. The whole §1.3 table was re-measured against `raku` before
any code was written and reproduced exactly as recorded, so §1.3 stands as measured.

**Slice 1.** `exec_for_loop_body` (`src/vm/vm_for_loop_body.rs`) now binds `array_slot_ref(idx, true)`
at the bind site when the parameter is a writable aliasing one (`is rw` / `<->` / `\v`), the loop has
a single parameter bound natively, and the tagged source is a direct, real, mutable, plain `Array`
(`@`-sigil, `!kv_mode`, `!values_mode`, `!loop_var_wraps_element`, `!container_reversed`). The
per-iteration `write_back_for_rw_param` is retired for exactly those iterations. Two helpers carry
the discriminator, `for_source_is_aliasable` and `for_element_alias`.

**Rows that turned green:** 01, 02, 03, 04, 07, 11, 12, 13, 14, 20, 27, 36 and 41 as planned, plus
**38** (the body rebinding the source array wholesale — the sharpest class-3 clobber) and **43** (the
sigilless `\v` read-alias, which §4 had filed under the topic slice but which binds through this same
native single-parameter site). Every invariant row still agrees.

**Perf (§1.5).** Release build, same machine, best of three, `for @a <-> $x { $x = $x + 1 }`:

| n | before | after |
| --- | --- | --- |
| 5 000 | 0.04 s | 0.01 s |
| 10 000 | 0.15 s | 0.01 s |
| 20 000 | 0.56 s | 0.02 s |
| 40 000 | 2.17 s | 0.03 s |
| 80 000 | 9.01 s | 0.05 s |
| 160 000 | 39.44 s | 0.11 s |

Before, each doubling multiplied the time by 3.7-4.4× — the quadratic §1.5 measured. After, it
roughly doubles. The quadratic is gone, not merely reduced. These are local A/B numbers recorded for
this ADR's own acceptance criterion; the bench CI series remains the source of truth for any headline
figure.

### Answers to the open questions, as measured by slices 1-3

- **§5 Q1 (ADR-0027 composition)** — no conflict, and slice 3 confirmed why rather than by luck. An
  `is rw` loop's parameter never enters `loop_local_vars` (`exec_for_loop_body` gates that set on
  `!spec.is_rw`), so slice 1's promoted cell could not reach `compute_owned_captures`'s unguarded
  primary branch. The **topic is not gated that way** — but it is excluded from `loop_param_names` by
  an explicit `name != "_"` filter, so it never enters that set either, and no guard was needed after
  all. Measured, not assumed: the topic form of rows 11/20 (a read-only closure over `$_` seeing a
  later element write) passes, and it is pinned in `t/for-loop-element-alias.t` precisely because it
  is what would break first if that filter ever changed. Rows 34/35 (per-iteration identity) and rows
  12/36 (sharing *within* an iteration) both hold, and `box_captured_lexicals` already refuses to
  double-box (`if self.locals[idx].is_container_ref() { continue }`).
- **§5 Q3 (ADR-0040 itemization)** — unchanged, deliberately: slice 1 flips a *local* writeback flag
  and never touches `spec.do_writeback`, which is what the bind-side itemization carve-out keys off.
  `t/param-bind-itemization.t` and `t/for-bind-typed-array-deitemize.t` pass unmodified.
- **§5 Q4 (leaked cells)** — none observed. Row 40 is extended in the pin to `.raku`, `.elems`,
  `.WHAT`, an element's own `.WHAT`, list context and interpolation; all decontainerize.
- **§5 Q5 (shaped / native-backed)** — kept as an explicit, commented carve-out in
  `for_source_is_aliasable`: `ArrayKind::Shaped`, an embedded `shape`, a `NativeBacking` payload and
  `ArrayKind::Lazy` all stay on the metadata-preserving writeback. `t/cas-shaped-and-for-loop.t` and
  row 26 pin it.
- **§5 Q6 (`Proxy` elements)** — the hazard does not evaporate by itself, and the fix is per
  *iteration*, not per loop. A `Proxy` element is skipped by the promotion, and because the writeback
  is retired per iteration rather than for the whole loop, that iteration keeps its writeback.
  Retiring it loop-wide silently dropped the `Proxy` element's write; the pin is the second Q6
  assertion in `t/for-loop-element-alias.t`.

### Slices 2 and 3 — landed 2026-08-27

**Slice 2 (hash sources).** `for %h.values -> $v is rw` (and its topic form) binds
`hash_slot_ref(key, true)`, keyed by the key order captured before the loop — the same order the
materialized `.values` list was built from. The key capture moves from *writeback* time to *bind*
time, which is strictly earlier and therefore strictly less exposed to a body that mutates the map.
`write_back_hash_value_item` survives only as the fallback for iterations that could not be promoted.
**Row 08 turns green**, along with the topic form and the hash sibling of rows 11/20 (a read through
the alias seeing a later `%h<a> = 5`).

**Slice 3 (the implicit topic, and the plain named parameter).** Two different jobs, as §4 predicted:

- **The topic promotes.** `for @a { … }` binds `$_` to the element container. **Rows 21, 42 and 44
  turn green**, and so does the topic form of rows 11/20.
- **The plain named parameter is a pure deletion.** `writes_back_named_param` is gone with nothing put
  in its place. **Row 22 turns green** — `for @a -> $v { @a[1] = 99 }`, silent corruption in code
  using no advanced feature at all, which was this pair of slices' acceptance criterion.

The discriminator moved into a single `plan_for_element_alias` (`src/vm/vm_for_loop_alias.rs`),
decided once per loop and carrying the resolved source.

**Three things the deletion exposed** — each a latent bug the writeback had been masking, and each a
pin in `t/for-loop-element-alias.t`:

1. **An `@`/`%`-sigil parameter did not bind *through* a `ContainerRef` element.** `for @c -> @row
   { @row.push(8) }` over an array whose elements are already cells (the rw-alias cells `.grep`
   leaves behind) pushed onto the *cell* rather than the row. The writeback re-stored the mutated
   binding over the element, hiding that the binding was never the container. Fixed at the bind site:
   a container-sigil parameter derefs a cell element. `t/for-loop-cell-elements.t` is the other pin.
2. **The kind test for "aliasable array" has to be a denial list.** ADR-0040 stores an `Array`
   element *itemized*, so the very common `for @m -> @row { for @row <-> $x { … } }` binds `@row` to
   an `ItemArray`. An allow list of `Array | List` dropped it back onto the writeback — which rebuilds
   a fresh `ArrayData` and **severs** `@row` from the `@m` element it shared. Invisible while the
   named parameter's writeback copied the severed array back; a lost mutation the moment that
   writeback went (row 37, plus a new row 37b that pins the sharing directly).
3. **A source tag does not prove the loop iterates that source's elements one-for-one.** `for @a,`
   builds a *one*-element list whose single item is the whole `@a`, yet still tags `@a`. Guarded by a
   **loop-entry** check (item count against element count, plus first-item identity). Deliberately
   *not* per iteration: the item vector is a loop-entry snapshot, so once the body has mutated an
   element it no longer matches — and that is precisely the case an alias must keep serving. A
   per-iteration version of this test silently re-broke rows 04, 21 and 38.

**Two topic-write paths had to learn to write *through* the binding**, since an aliasing topic is no
longer a plain value: the destructive `s///` writeback (`vm_subst_exec.rs`) and the `$_ ~~ s///`
smartmatch writeback (`vm_smartmatch_ops.rs`), which additionally must not install the
decontainerized LHS over the cell for the duration of its RHS. Pins: `t/subst-readonly-topic.t`,
`t/smartmatch-subst-topic.t`, `t/statement-modifier-for-regression.t`.

**Perf.** Release build, best of three. The topic loop carried the same quadratic the `<->` loop did,
and `%h.values` carried a worse one:

| probe (n) | before slices 1-3 | after |
| --- | --- | --- |
| `for @a { $_ = $_ + 1 }` (20 000) | 0.71 s | 0.02 s |
| `for @a { $_ = $_ + 1 }` (160 000) | 43.14 s | 0.16 s |
| `for @a <-> $x { $x = $x + 1 }` (160 000) | 39.83 s | 0.16 s |
| `for %h.values -> $v is rw { … }` (20 000) | 17.32 s | 0.04 s |

**§5 Q2's predicted cost is real, and here it is.** A *read-only* topic loop pays for a promotion
nothing observes: 160 000 elements go from 0.07 s to 0.24 s on the first pass. Splitting the two
costs (four passes over the same array, where `array_slot_ref` is idempotent) puts ~0.12 s of that in
the first pass's per-element cell allocation and ~0.12 s in the steady-state per-iteration source
resolution plus cell deref — against 0.05 s for the equivalent named-parameter loop. The repository's
own benchmarks do not move (`bench-array` 0.03→0.02, `bench-hash` 0.03→0.02, `bench-class`
0.15→0.17, `bench-string`/`bench-ctor`/`bench-mandelbrot` unchanged), so this is a synthetic-shape
cost rather than a corpus-wide one — but it is the honest answer to Q2, and the bench CI series
remains the place to watch it. A lazy "promote only when the binding is observed" scheme would remove
it and needs its own design; do not bolt on a cached source resolution to chase it, since
re-resolving per iteration is what makes row 38 work.

### Slice 4 — landed 2026-08-27 (with ADR-0036 slice 3)

§4 required these to land together because they are the same producer layer, and the shared layer is
real: `src/vm/vm_element_producers.rs`, hooked into the VM's method-dispatch tail, makes
`.values`/`.reverse`/`.sort` hand out the elements' own `Scalar` containers when the receiver is a
real mutable `Array`/`Hash`.

ADR-0036's own `.pairs` did **not** ship with it. It was implemented on this same layer, measured,
and backed out: a Pair carrying a cell leaks into the many consumers that read a pair's value *as
data*, and five distinct failures were measured before that was accepted as a class (see ADR-0036's
slice-3 status). A flat list of cells — which is what `.values`/`.reverse`/`.sort` produce — has no
such problem, because list consumers decontainerize. So the "two half-converted method sets" §4
warned about did not materialise in the direction it expected: the split is not array-producers vs
pair-producers, it is **flat lists vs Pair wrappers**, and that split is now a measured property
rather than a scheduling accident.

**The index bookkeeping was deletable, not fixable — and this is why.** Once a producer hands out
element containers, the loop needs no index reconstruction at all: the item it binds *is* the alias,
in whatever order the producer chose. So the loop's rule is simply "if the bound item already carries
an element cell, there is nothing to write back" (`binding_carries_element_cell` in
`vm_for_loop_body.rs`), and `container_reversed` stops being a mirror-image index to compute.
`.sort` gets an alias at all this way, having no index to reconstruct — the cells are carried
*through* the sort, keyed by what they hold, rather than an index being recovered from the sorted
value afterwards (which is ambiguous the moment two elements compare equal).

**Rows 17 and 24 turn green**, plus the deferred-closure form of each and of `@a.values`.

**Row 39 (`for @$s`) is green.** It did not need the producer layer at all — its `$`-tagged source is
an ordinary in-order array read, so it joins slices 1-3's bind-site routing via a shared
`resolve_for_source_array`.

It was implemented, backed out once, and re-landed, and the back-out's diagnosis was wrong in a way
worth recording. The symptom was CBOR::Simple encoding a `Map` as an integer under
`encode($_) for @$_`, which read as the type-test hazard this section warns about — a promoted cell
reaching `nqp::istype($_, Associative)`. It was not. **The `$`-tagged source was being re-resolved by
name on every iteration**, and the name in the recursive-walk idiom is `$_`: any nested loop in the
body rebinds the topic, so iteration `n+1` aliased into whatever container the *inner* loop had been
walking. The `Map` was never type-tested as a cell; the loop simply bound the wrong element (the
inner list's `[1]`, an `Int`). Re-resolution is correct for the `@a` shape — a body may assign the
array wholesale — but wrong for this one, because `for @$s` derefs `$s` exactly once to choose the
array it walks. `ForElementAlias::ArrayValue` captures the resolved array at loop entry instead.

The `nqp::` decontainerization the back-out started is kept and generalized: `call_nqp_op`
decontainerizes its operands once at the boundary rather than op by op, since no `nqp::` op wants a
Raku container. That is a real hardening for promoted values reaching the NQP layer — it just was not
what row 39 was failing on.

**Row 16 (`.kv`) is deferred, and the reason is the consumer.** A `.kv` loop is a *multi-parameter*
loop, and those do not bind at the native bind site — they bind through the bind-prefix
`Stmt::Assign`s, each reading its chunk element through the ordinary element chokepoint, which
**decontainerizes**. Routing `.kv` therefore *lost* the direct write (row 15,
`for @a.kv -> $i, $v is rw { $v += $i }`) rather than gaining the deferred one, because the writeback
that used to carry it had been retired for the iteration. It needs a raw bind for an rw scalar
multi-parameter first — the shape `@`/`%`-sigil multi-params already have via `Stmt::MarkBind`.
Tracked in `todo/tickets/for-kv-multi-param-bind-decontainerizes.md`. This is the "hard half" §8
predicted, and the prediction was right for a reason nobody had written down: the difficulty is not
in `.kv` at all.

**Perf.** Eager promotion costs a read-only producer pass on a large container (200 000 elements,
release, best of three, idle machine): `.values` 0.07 s → 0.18 s, `.reverse` 0.07 s → 0.18 s,
`.sort` 0.05 s → 0.18 s. The unrouted producers are the controls and stay flat (`.pairs` 0.23 →
0.25 s, `.kv` 0.73 → 0.77 s). The repository's own benchmarks do not move. See ADR-0036's slice-3
status for the full table.

**The hazard in this campaign is type-testing a promoted value, not reading one**, and ADR-0036 §5
Q5 predicted it. `"...".trans(%matcher.pairs)` type-tests the pair value (`is_closure`, then a
Regex/Array/Range shape match) and a `ContainerRef` answers "no" to all of them. Four more of the
same shape followed, all of them fed by `.pairs`. Slice 5 should assume that any *new* place a cell
can reach will be found this way — by a full roast sweep, not by reading — and that the tell is a
`match` on `view()`, not a `.value` read.

### Slice 5, the bind-time rejection half — landed 2026-09-01

Rows 19 and 30 are green: `for (1, 2) -> $v is rw { }` and `for 1 .. 2 -> $v is rw { }` now fail the
**bind**, before the body runs, with raku's own `X::Parameter::RW` and its exact wording (embedded
newline included) — where mutsu used to bind a value clone and silently drop the write.

**The gate is the source, not the promotion.** The obvious implementation — "the item was not
promoted to a cell, so reject the `is rw` bind" — is wrong, and measurably so: `for flat(@a)` and
`for @a[0, 1]` also fail to promote today (their producers are unrouted), yet raku *aliases* through
both. Keying the rejection off promotion would have traded a lost write for a spurious death, which
is the worse divergence. It is keyed instead on the compiler's conservative
`ForLoopSpec::source_items_are_bare` — a literal list, a word list, `%h.keys`, and (added here) any
`Range` — which answers `true` only for shapes that can never produce a container. Sources raku also
rejects but the flag does not yet see (`@a.map(...)`, `.List`, `.Seq`, a sub's return, `%h`'s Pairs)
keep the old silent behaviour; widening the flag is additive and needs no further decision.

A sigilless `\v` parameter is excluded, because raku treats it differently: it binds the bare item
happily and only dies if the body *assigns* through it ("Cannot modify an immutable Int"). The AST
stores `\v` as plain `"v"`, so the name cannot carry that distinction — `ForLoopSpec` gained
`param_sigilless` for it.

**The wording was shared, not duplicated.** `RuntimeError::parameter_rw_not_container` now backs both
this bind site and the two routine-signature sites in `runtime/types/binding_signature.rs`, which had
been raising an invented `X::Parameter::RW: 'x' expects a writable variable argument` (no sigil, no
`.symbol`/`.got`, not an exception instance at all). `sub f($x is rw) {}; f(1)` matches raku's message
now too.

**Row 28 landed separately** — the element type constraint belongs to the promoted cell, so ADR-0036
slice 4 owns it, and it arrived in two pieces: the check with #7190, the raku wording (`for an
element of @a`, naming the container rather than the alias) on 2026-09-01. The loop's contribution to
the second piece is a retag — it has already resolved its source name, so it tells the cell which
container to blame. See ADR-0036's slice-4 status.

### What slices 5-6 still own

Row 16 (`.kv`), carried over from slice 4 — see above. It is the only `todo`-marked row left in
`t/for-loop-element-alias.t`; rows 19/30 and 28 all landed 2026-09-01. The writeback family survives only as the fallback for shapes not yet converted:
`write_back_for_rw_param`'s `kv_mode`, multi-parameter and scalar arms, and
`write_back_hash_value_item` for a hash iteration that could not be promoted.

**Row 28 turned green on 2026-09-01**: the promoted element cell carries its array's `value_type`
(landed with ADR-0059's bare-tail half, `news/2026-09/is-rw-bare-tail-returns-container.md`), so
slice 5 keeps only rows 19/30 (bind-time immutable-source rejection). The note below is kept for
the record.

**A note for slice 5, which is shared three ways.** The element type constraint (row 28) is also
ADR-0036 slice 4's and ADR-0042's; whichever lands it first owns it. **ADR-0036 slice 4 is the
natural owner**: it already has to touch `methods_mut_method_lvalue.rs` to delete the env-scan
compensator, its `lookup_container_constraint` call site is where the constraint is consumed, and the
gap exists for `:=`-bound elements today independently of any `for` loop
(`my Str @a; my $r := @a[0]; $r = 42` wrongly succeeds). This ADR's slice 5 should consume it rather
than build it, and keep only the bind-time immutable-source rejection (rows 19/30), which is
genuinely the loop's own.

**`.sort` did not need a source tag after all.** §8 recorded that `for_iterable_source_name` does not
match `.sort`, and treated that as a prerequisite. Routing the producer made it moot: the loop binds
the cell the producer handed out, so there is nothing for a tag to point at. The same is true of
`.reverse` — `container_reversed` survives only for the shapes that still take the writeback.

**Found along the way, unrelated:** after any `start` block, a later `for @m -> @row { ... }` loop
rebinds the *previous* iteration's container. Pre-existing on `main` (verified at `f678b032b`) and
independent of this ADR — recorded as
`todo/tickets/at-sigil-for-param-rebinds-stale-container-after-start-block.md`. It is why
`t/for-loop-element-alias.t` keeps its `start`-block row last.

---

*Slices 0-4 of this ADR are implemented; the decision stands. If the mechanism judgment changes
later, supersede it rather than rewriting it.*
