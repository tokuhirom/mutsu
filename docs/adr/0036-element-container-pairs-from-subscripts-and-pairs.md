# ADR-0036: A Pair produced by a subscript adverb or `.pairs` carries the element *container*, not a snapshot

- **Status**: Partially implemented — slices 1-2 landed (2026-08-20); slice 3's producer layer landed
  2026-08-27 but `.pairs` itself is deferred (see "Implementation status — slice 3"); slice 4's
  enforcement half landed with #7190 and its reporting half 2026-09-01, its compensator deletion is
  still open
- **Date**: 2026-08-20
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0013](0013-container-interior-mutability-cellvalue.md) §5 open question 3 (element-level cells, "2c / Track B proper" — deferred there, and this ADR is the correctness driver that reopens it in a scoped form), [ADR-0001](0001-gc-strategy-and-phasing.md) (layer 3a / Track B framing), [ADR-0021](0021-argument-namedness-is-a-call-site-property.md) (Pair flavour unification — `.pairs`' output is data, not a call site), `todo/deep/subscript-p-pair-is-a-snapshot-not-a-container.md` (the originating finding)

> Raku's `@a[0]:p` is `0 => @a[0]` where the *value is the element's `Scalar` container*. mutsu builds
> that Pair from a snapshot of the element and then compensates for the missing link with a runtime
> search of `self.env` for an array whose element happens to compare equal. This ADR decides to
> retire that search by producing the Pair from the element container that mutsu already has a
> primitive for, and scopes which producers convert.

---

## 1. Context

### 1.1 What Raku specifies

An `Array`/`Hash` element is a `Scalar` container, and every construct that hands an element out as a
`Pair` value or a `kv` list element hands out *that container*, not a copy of what it holds:

```
$ raku -e 'my @a = <A B>; say @a[0].VAR.^name'                    # Scalar
$ raku -e 'my @a = <A B>; say (@a[0]:p).value.VAR.^name'          # Scalar
$ raku -e 'my @a = <A B>; say (@a[0]:kv)[1].VAR.^name'            # Scalar
$ raku -e 'my @a = <A B>; say (@a.pairs)[0].value.VAR.^name'      # Scalar
```

Two consequences follow directly, and both are observable:

- **Writing through the pair updates the array** — `(@a[0]:p).value = 'x'` sets `@a[0]`, with no
  search for the array involved: it is an ordinary container assignment.
- **Reading through the pair sees later writes to the array** — `my $p = @a[0]:p; @a[0] = 'Q';
  $p.value` is `Q`.

Where the source is *not* a mutable container the pair value is an immutable item and the write must
die: `(1,2).pairs[0].value = 3` raises `Cannot modify an immutable Int (1)`.

### 1.2 What mutsu does

The pair producers build the Pair from a cloned element:

- `builtins_multidim_subscript.rs:528`, `:655`, `:672`, `:739` — every `"p" => Value::value_pair(key, value)`
  arm of the subscript-adverb machinery (`:p`, and the `:kv` list rows next to them).
- `builtins/methods_0arg/mod.rs:815` and `collection.rs:598` — `.pairs` / `.kv` / `.antipairs`, which
  map over an already-decontainerized `&[Value]` with `v.clone()`.

So the Pair holds no link back to its source. To make `.value = X` work anyway,
`assign_method_lvalue_with_values` (`runtime/methods_mut_method_lvalue.rs:462`…) *searches for* the
backing container at assignment time: it scans `self.env.values()` for a `Hash` whose entry at the
pair's key, or an `Array` whose element at the pair's integer key, compares `==` to the pair's value
(`:573-606`), requires the match to be unique by `Gc::ptr_eq`, rebuilds the whole container, and
writes it back by identity (`overwrite_array_bindings_by_identity` / `overwrite_hash_bindings_by_identity`).
Adjacent arms extend the same trick to shaped arrays by index tuple (`:613-646`), to mutable
QuantHash weights via `topic_source_var` (`:529-546`), and — a fourth consumer, easy to miss — to
*standalone* Pairs at `:686-723`, which scans `self.env.iter()` for every binding holding a Pair with
the same key and the same old value and rebinds each one, "simulating Raku container semantics where
pair values are aliases" (its own comment). That is the same search under a different name, so it
belongs to the same deletion.

One arm above it, `:445-461`, keys off a Pair `Instance` carrying a `__mutsu_hash_ref` attribute.
**Nothing in the tree ever writes that attribute** — it is read at exactly two places
(`methods_mut_method_lvalue.rs:454` and `builtins/methods_0arg/coercion.rs:212`) and constructed
nowhere. It is dead and should be removed with the rest.

### 1.2b `:kv`'s write path is a parser rewrite, not a runtime route

The originating finding noted that `(@a[0]:kv)[1] = 'x'` works today "by a different route" and asked
for it to be audited alongside. It is worth stating precisely what that route is, because it is not a
runtime mechanism at all — **the parser rewrites the whole construct into a plain element assign**,
so no adverb machinery and no Pair is ever built. Verified with `--dump-bytecode`:

```
$ mutsu --dump-bytecode -e 'my @a = <A B>; (@a[0]:kv)[1] = "x";'
    7: IndexAssignExprNamed { name_idx: 0, is_positional: true, target_slot: Some(0) }

$ mutsu --dump-bytecode -e 'my @a = <A B>; (@a[0]:p).value = "x";'
    7: Str("__mutsu_subscript_adverb")
   10: Str("__mutsu_assign_method_lvalue")
```

The rewrite matches `Call("__mutsu_subscript_adverb", [target, index, mode, …])` with `mode ∈ {"kv",
"not-kv"}` and a literal index of `1`, and rebuilds it as `Expr::IndexAssign`. It is implemented
twice — once for statement position (`src/parser/stmt/assign/lvalue.rs`) and once for expression
position (`src/parser/expr/precedence/logic.rs`), the latter being what makes it work inside a
closure. There is no `.value` counterpart in either, so the `:p` form always lands in the runtime
scan.

Two consequences for this ADR. First, `:kv`'s apparent health is confined to that one syntactic
shape: its *read* direction is as stale as `:p`'s (§1.3 row 2), and anything that stores the `:kv`
list first is unaffected by the rewrite. Second, once slice 2 gives `:kv` real element containers,
**the rewrite becomes redundant and should be deleted with it** — leaving both means one syntactic
form takes a fast path and every other form takes the semantic one, which is exactly the kind of
divergence that hides bugs.

### 1.3 The divergence this produces — measured on `main` (e13d278ff)

| program | raku | mutsu |
| --- | --- | --- |
| `my @a = <A B>; my $p = @a[0]:p; @a[0] = "Q"; say $p.value` | `Q` | `A` |
| `my @a = <A B>; my $kv = @a[0]:kv; @a[0] = "Q"; say $kv` | `(0 Q)` | `(0 A)` |
| `my @a = <A B>; my $p = @a.pairs[0]; @a[0] = "Q"; say $p.value` | `Q` | `A` |
| `my %h = a => 1; my $p = %h.pairs[0]; %h<a> = 7; say $p.value` | `7` | `1` |
| `my @a = <A B>; my $p = @a[0]:p; $p.value = "x"; say $p.raku` | `0 => "x"` | `0 => "A"` |
| `my @a = <A B>; say (@a[0]:p).value.VAR.^name` | `Scalar` | `Str` |
| `my @a = <A B>; my @b = @a; (@a[0]:p).value = "z"; say @a` | `[z B]` | **dies** `X::Assignment::RO: cannot assign through .value on non-instance` |
| `my %h = a => 1; my %g = a => 1; (%h<a>:p).value = 9; say %h` | `{a => 9}` | **dies**, same error |
| `my @a = <A B>; my @c = <A B>; for @a.pairs -> $p { $p.value = "y" }; say @a` | `[y y]` | `[A B]` — **silent no-op** |
| `my @a = <A B>; my $p = 0 => @a[0]; $p.value = "x"` (with a sibling `my @c = <A B>`) | `[x B]` | `[A B]` — silent no-op |
| `my $l = (1,2); $l.pairs[0].value = 3` | dies `Cannot modify an immutable Int (1)` | silently succeeds as a no-op |
| `my Str @a = <A B>; (@a[0]:p).value = 42` | dies `Type check failed for an element of @a` | `[42 B]` |

Read the failure column as three distinct classes:

1. **Stale reads** (rows 1-5). No widening of the search can fix these — the search runs at
   *assignment* time and there is no assignment. Only a live container can.
2. **Ambiguity failures** (rows 7-10). The uniqueness guard is defeated by any second container in
   scope that happens to hold an equal value at the same key — including `my @b = @a`, an ordinary
   copy. The `:p` form then *dies* with a misleading `X::Assignment::RO ... on non-instance` (control
   falls out of the whole `method == "value"` arm and reaches the generic instance-attribute path);
   the `.pairs` form *silently does nothing*, which is worse.
3. **Missing enforcement** (rows 11-12). A rebuild-and-reinsert write bypasses both the immutability
   of a `List` source and the element type constraint of a typed array.

The originating finding (`todo/deep/…`) reported class 2 through a much narrower repro — a bare block
whose `@a` is a *local slot* rather than an env entry because a later sibling block redeclares the
name, so the env scan matches nothing at all. That repro still reproduces verbatim, but it is a
special case of the same defect, not the defect.

### 1.4 What already exists — and why this is not "Track B is missing"

The originating finding concluded "there is no `array_element_cell`-style API today" and that the fix
therefore waits on ADR-0001's Track B. **That is out of date.** The element-container primitive
shipped and is in daily use on the binding path:

- `Value::array_slot_ref(idx, terminal)` (`value/value_methods_b.rs:94`) replaces an array element in
  place with a shared `ContainerRef` cell (reusing one already there) and returns that same cell, so
  the alias is by cell identity and survives COW clones of any enclosing container. `Value::hash_slot_ref`
  (`value/value_methods_a.rs:603`) and `hash_autovivify_cell` (`:558`) are its hash analogues.
- Reads decontainerize at single chokepoints — `resolve_array_entry` (`vm/vm_var_ops.rs:123`, the
  `ValueView::ContainerRef(cell) => cell.lock().unwrap().clone()` arm at `:147`) and `resolve_hash_entry`
  — so a promoted slot is invisible to value contexts.
- `assign_method_lvalue_with_values` already assigns through a `ContainerRef` pair value
  (`methods_mut_method_lvalue.rs:496-521`), frozen-check and `of`-type constraint included. That arm
  is what makes `key => $var` pairs write through today.
- The `for %h -> $p` loop already yields pairs whose value is a live `HashEntryRef`, handled in place
  at `:481-490`.

And it demonstrably works end to end:

```
$ mutsu -e 'my @a = <A B>; my $r := @a[0]; $r = "x"; say @a'                  # [x B]
$ mutsu -e 'my @a = <A B>; my $r := @a[0]; @a[0] = "Q"; say $r'               # Q
$ mutsu -e 'my @a = <A B>; my $r := @a[0]; my @c = <A B>; $r = "x"; say @c'   # [A B]  (no ambiguity)
$ mutsu -e 'my @a = <A B>; my $r := @a[0]; say @a.raku'                       # ["A", "B"]  (invisible)
```

So the missing piece is **not** a primitive. It is that the pair *producers* never ask for the slot,
and that a search-based compensator was grown next to them instead. This ADR is therefore a scoping
and sequencing decision over an existing mechanism, not the start of a new campaign — which is
precisely the state ADR-0013 §5 Q3 anticipated when it resolved "land 2b first, then pursue 2c
incrementally where it measurably reduces container-level structural sites". This is the first place
where 2c buys *correctness* rather than soundness tidiness, so it is the natural first increment.

### 1.5 Why the producers cannot simply be patched in place

`.pairs` / `.kv` / `.antipairs` live in the arity-dispatched fast path (`builtins/methods_0arg/`),
which is value-in/value-out by construction: it receives `&[Value]` — the *decontainerized* items —
and has neither the invocant container's identity nor the ability to mutate it. Producing an element
container needs both (promotion is an in-place write into the container's storage). The subscript
adverbs (`builtins_multidim_subscript.rs`) are closer to the container but are likewise pure
functions over a resolved target.

That is the real reason this is an architecture decision rather than a four-line edit: the change has
to move the decision of *what a pair value is* to a layer that holds container identity, and it has
to do so without regressing the fast path for the overwhelming majority of `.pairs` calls that only
read.

---

## 2. Decision

**Produce element-container Pairs at the container-aware layer, discriminated by the source's
mutability, and delete the env-scan compensator.**

Three parts:

1. **A container-aware production path.** For a *mutable container* invocant/target — `Array`
   (any `ArrayKind`), `Hash`, and the shaped-array multidim forms — `:p`, `:kv`, `.pairs`, `.kv`, and
   `.antipairs` produce their value elements by `array_slot_ref(i, true)` / `hash_slot_ref(k, true)`
   rather than by cloning the element. The producer therefore lives in the VM method/subscript
   dispatch layer (`vm/vm_call_method_mut_ops.rs`, `runtime/builtins_multidim_subscript.rs`'s
   callers), with the existing pure-value implementation retained as the fallback.

2. **Mutability is the discriminator, and it is the whole of the immutability story.** A `List`,
   `Seq`, `Range`, `Capture`, `Match`, or immutable `Set`/`Bag`/`Mix` invocant keeps the snapshot
   producer. A snapshot pair value is a bare item, so `.value = X` on it reaches the existing
   read-only guard and dies with `Cannot modify an immutable <T>` — which is exactly raku's behaviour
   for `(1,2).pairs[0].value = 3` and requires no new check. Mutable QuantHashes (`SetHash`/`BagHash`/
   `MixHash`) keep their weight-writeback arm: a weight is not a stored element container and has
   removal semantics at 0, so it is genuinely a different operation (see §5 Q2).

3. **The env scan goes away.** `methods_mut_method_lvalue.rs:547-668` — `selected_hash` /
   `selected_array`, the `self.env.values()` candidate scans, the shaped-array index-tuple scan, and
   the `overwrite_*_bindings_by_identity` rebuild — is deleted once every producer that feeds it has
   converted, and so are its two outriders: the standalone-Pair env rebind at `:686-723` and the dead
   `__mutsu_hash_ref` branch at `:445-461` (§1.2). The `:kv` parser rewrite (§1.2b) goes with them.
   What remains in the `method == "value"` arm is the `ContainerRef` assignment at `:496` (which
   gains the element type constraint, §4 slice 4), the `HashEntryRef` in-place write at `:481`, the
   QuantHash weight arm, and the read-only guard.

### Why this direction

- **It is the only direction that fixes stale reads.** Classes 1 and 2 of §1.3 have a single common
  cause and a single fix; a wider or smarter search addresses at most class 2, and would deepen a
  mechanism that is already wrong.
- **It removes a heuristic rather than adding one.** The scan's uniqueness guard is not a
  conservative approximation that errs safe — it errs into a *misleading exception* on one path and a
  *silent no-op* on another, both triggered by an ordinary array copy. Under the project's gain/risk
  definitions this is squarely a gain: a value-equality search over the environment is exactly the
  kind of incomplete static reasoning that cannot be made sound by refinement.
- **It reuses a shipped, proven primitive.** `array_slot_ref`/`hash_slot_ref` are already exercised by
  every `:=`-bound element, already invisible to `.raku`/`.elems`/copy, and already survive COW. The
  new work is routing, not invention.
- **It closes an enforcement hole for free.** Assigning through a real cell runs the frozen check and
  the constraint check that the rebuild path skipped entirely (row 12).
- **It is the increment ADR-0013 §5 Q3 asked for**, with a correctness justification attached, so it
  does not re-open the deferred general 2c migration: only element *reads that hand the element out*
  promote, not every element.

---

## 3. Options considered

| Option | Fixes stale reads | Fixes ambiguity | Fixes immutability/constraints | Blast radius | Verdict |
| --- | --- | --- | --- | --- | --- |
| **Status quo (search `self.env`)** | ✗ | ✗ | ✗ | — | Rejected — the defect |
| **A. Widen the search to `self.locals`** | ✗ | ✗ (worsens: more equal candidates → more declines) | ✗ | tiny | **Rejected.** It repairs only the originating finding's narrow repro and makes the ambiguity class strictly more likely. |
| **B. Back-reference Pair (hold `Gc<ArrayData>` + index in the Pair)** | ✓ | ✓ | partial | medium | Rejected — this is the `HashEntryRef` design, and `array_slot_ref`'s own doc records why it was abandoned: a back-reference goes stale when an enclosing container is COW-cloned on a later write. Re-adopting it for arrays would reintroduce a bug class that cell identity already solved. |
| **C. Element containers at the producer, mutability-discriminated (this ADR)** | ✓ | ✓ | ✓ | medium | **Chosen** |
| **D. Full 2c: every element is a cell, always** | ✓ | ✓ | ✓ | very large | Deferred, not rejected. It subsumes C, but pays a cell per element on construction and forces every `ValueView::Array(arr, _)` consumer that inspects elements to decontainerize. C reaches the same observable semantics for the constructs that need them, and it is the honest first measurement of what D would cost. |

Option D's cost is the reason C promotes **lazily, per element handed out**, exactly as the binding
path does. `@a.pairs` over an N-element array promotes N slots; `@a[0]:p` promotes one.

---

## 4. Phasing

Each slice is independently landable and independently green.

1. **Slice 1 — pin the semantics.** Add `t/subscript-pair-element-container.t` covering every row of
   §1.3 as a *currently failing* expectation set, fudged/skipped as needed so it lands green, plus the
   `.VAR.^name` probes. This is the acceptance oracle for slices 2-4 and prevents the campaign from
   being declared done on the write direction alone. The existing `t/subscript-adverbs.t`,
   `t/pairs-value-writeback-array-kind.t`, and `t/for-pairs-value-quanthash-writeback.t` are the
   regression pins — all three pass today *only* via the scan, so they are precisely what must keep
   passing after it is deleted.

2. **Slice 2 — subscript adverbs.** Route the `:p` and `:kv` arms in
   `runtime/builtins_multidim_subscript.rs` (`:528`, `:655`, `:672`, `:739`) through slot refs when
   the target is a mutable container, and retire the `:kv` parser rewrite (§1.2b) in the same slice —
   its two sites are `src/parser/stmt/assign/lvalue.rs` and `src/parser/expr/precedence/logic.rs`,
   and keeping it would leave one syntactic form on a fast path while every other form takes the
   semantic one. Rows 1, 2, 5, 6, 7, 8 and 12 of §1.3 turn green. `t/subscript-adverbs.t` is the pin
   that the rewrite's removal must not regress.

3. **Slice 3 — `.pairs` / `.kv` / `.antipairs`.** Add the container-aware path at the VM method
   dispatch layer, keeping `builtins/methods_0arg/` as the immutable-source fallback. Rows 3, 4, 9,
   10 and 11 turn green.

4. **Slice 4 — enforcement and deletion.** Teach the promoted cell the container's element
   constraint (`ArrayData::value_type` / `HashData::value_type`, `src/value/mod.rs`) so
   `lookup_container_constraint` at
   `methods_mut_method_lvalue.rs:508` sees it — note this gap exists for `:=`-bound elements today
   too (`my Str @a; my $r := @a[0]; $r = 42` wrongly succeeds), so the slice fixes both at once. Then
   delete `methods_mut_method_lvalue.rs:547-668`, the standalone-Pair rebind at `:686-723`, the dead
   `__mutsu_hash_ref` branch at `:445-461` (and its only other reader,
   `builtins/methods_0arg/coercion.rs:212`), and any `overwrite_*_bindings_by_identity` helper left
   with no callers.

5. **Slice 5 — sweep.** Re-run the §1.3 table, run the whitelisted S02/S09/S32 roast families, and
   record the outcome in this ADR's "Implementation status".

---

## Implementation status (updated 2026-08-20)

**Slices 1-2 landed.** `t/subscript-pair-element-container.t` pins all twelve §1.3 rows (plus the two
`.VAR.^name` probes from §1.1), `todo`-marking the rows that need slice 3 or 4. `:p` and `:kv` in
`builtin_subscript_adverb` (`runtime/builtins_multidim_subscript.rs`) now hand out
`array_slot_ref`/`hash_slot_ref` element containers instead of snapshots when the source is a genuine
mutable Array/Hash (not a List/Range/Seq coercion, not a QuantHash `.hash` projection, not an
AT-KEY-instance snapshot) — covering both the single-index and slice/nested-index shapes. The `:kv`
parser rewrite (§1.2b) is deleted from both sites (`src/parser/stmt/assign/lvalue.rs`,
`src/parser/expr/precedence/logic.rs`); `(@a[0]:kv)[1] = x` now reaches the same outcome through the
ordinary index-assign write-through-a-`ContainerRef` path (`Value::assign_element_slot`) instead of a
syntax-specific rewrite.

- **Rows 1, 2, 5, 6, 7 and 8 of §1.3 turn green** (six rows, not seven — see the row 12 correction
  below). Verified against `raku -e` for each row plus the ambiguity/ `:kv` variants exercised in the
  new test file.
- **Row 12 turned green on 2026-09-01** — the promoted cell now carries `ArrayData::value_type` /
  `HashData::value_type` (`array_slot_ref` / `hash_slot_ref` register it, `assign_lvalue_container`
  checks it), landed with ADR-0059's bare-tail half
  (`news/2026-09/is-rw-bare-tail-returns-container.md`). What is left of slice 4 is the
  `methods_mut_method_lvalue.rs` env-scan compensator deletion. The original slice-2 note follows.
- **Row 12 does NOT turn green in slice 2**, correcting §4's phase list above: enforcing the typed
  array's element constraint requires teaching the promoted cell about `ArrayData::value_type` /
  `HashData::value_type` (`register_container_constraint`), which is explicitly slice 4's job. Slice 2
  only routes the *value*, not the constraint, through the cell, so `(@a[0]:p).value = 42` on a
  `Str @a` still succeeds silently pending slice 4. `t/subscript-pair-element-container.t` marks this
  row `todo` with a comment pointing at slice 4.
- **Rows 3, 4, 9, 10 and 11 remain slice 3 territory**, unchanged and `todo`-marked, including row 10
  (`key => @a[i]`) — the FatArrow `key => $var` container-capture optimization
  (`compiler/expr_binary.rs`'s `WrapVarRef`) only recognizes a bare `Expr::Var` RHS today, not an
  arbitrary Index expression, so extending it belongs with the rest of slice 3's container-aware
  producers rather than slice 2's subscript-adverb-only scope.
- **A `:delete` companion needed an extra gate.** The first slice-2 pass promoted `:p`/`:kv` array rows
  unconditionally whenever the source index existed pre-delete, which broke
  `roast/S09-subscript/slice.t`'s "Nested slice, delete + p/kv adverbs" subtests: the array branch
  applies `:delete` (overwriting the live slot with a hole) *before* formatting the adverb's rows, so
  promoting *after* delete handed back a container around the fresh hole instead of the pre-delete
  snapshot value the adverb must report. Fixed by skipping the container-aware path whenever
  `delete_after` is set (both the single-index and `format_positional_slice_level` slice/nested paths),
  falling back to the plain snapshot value there — matching raku, where a deleted slot has nothing left
  to alias.
- The shaped-array multidim `:p`/`:kv` form (§5 Q3) was **not** converted in this pass — it lives in a
  separate function (`runtime/builtins_multidim_ops.rs`'s `builtin_multidim_subscript_adverb`), not the
  four `builtins_multidim_subscript.rs` sites this slice targeted, and none of the twelve §1.3 rows
  exercise it. Left for a follow-up alongside slice 3/4; the shaped-array env-scan
  (`methods_mut_method_lvalue.rs:613-646`) stays until then.
- Regression pins verified green: `t/subscript-adverbs.t`, `t/pairs-value-writeback-array-kind.t`,
  `t/for-pairs-value-quanthash-writeback.t`, the roast S02/S03/S06/S09/S12/S29/S32 pair/subscript/delete
  families, and the full local `t/` suite (`make test`).
- **Next**: slice 3 (`.pairs`/`.kv`/`.antipairs` at the VM method dispatch layer, plus extending
  FatArrow's container capture to an Index RHS for row 10) and slice 4 (element type constraint on the
  promoted cell, then deleting the `methods_mut_method_lvalue.rs` env-scan compensator).

## Implementation status — slice 3 (2026-08-27)

**The producer layer landed; `.pairs` itself did not.** §4 required this slice to land with ADR-0045
slice 4, and the mechanism they share does exist now: `src/vm/vm_element_producers.rs`, hooked into
`exec_call_method_mut_op_impl` (`src/vm/vm_call_method_mut_ops.rs`) just before the native dispatch
tail, with `builtins/methods_0arg/` kept as the fallback for every receiver it declines. ADR-0045
slice 4's `.values`/`.reverse`/`.sort` ship through it. **`.pairs` was implemented, measured, and
backed out**, so rows 3, 4 and 9 stay `todo`-marked.

**Why `.pairs` was backed out — and why `.values`/`.reverse`/`.sort` were not.** Handing out a Pair
whose *value* is a cell leaks into every consumer that reads a pair's value **as data**, and because
`.pairs` promotes the source's elements in place, the exposure is not "consumers of the `.pairs`
result" but "consumers of any container a producer has run over". Five distinct leaks were measured
before the pattern was accepted as a class: `trans` type-testing the value
(`roast/S05-transliteration/with-closure.t`), Hash-from-pairs aliasing two hashes together,
BagHash-from-pairs collapsing every weight to 1 (`roast/S03-metaops/infix.t`, 396/5076 subtests),
`.map({.key => .value})` carrying the cell forward, and `.antipairs` losing its key de-itemization.
`set_coerce.rs` and `coerce_containers.rs` alone destructure a pair's value structurally in **15**
places, with no accessor to route. `.values`/`.reverse`/`.sort` do not have the problem: they hand
out a *flat list* of cells, and list consumers decontainerize. It is specifically the **Pair
wrapper** that carries a cell into code that reads it structurally. Tracked in
`todo/deep/pairs-element-containers-leak-through-pair-value-consumers.md`.

**This revises §5 Q4's answer.** `resolve_array_entry` is the only chokepoint for *element* reads,
but bulk iteration (`h.iter()`, `items.iter()`) and `ValueView::Pair(k, v)` destructuring walk the
storage directly and bypass it. That is the real blast radius, and it is why the `:p`/`:kv` adverbs
(slice 2) have shipped happily since 2026-08-20 while `.pairs` cannot: they promote **one** element
on demand, where `.pairs` promotes the whole container and is far likelier to be fed to a coercion.

**Two corrections to §4's method list, both by measurement:**

- **`.antipairs` must NOT be routed.** It puts the element in the Pair's *key*, and a Pair key is
  never a container in raku: `my @a = <A B>; my $p = @a.antipairs[0]; @a[0] = "Q"; $p.key` is `A`,
  and `$p.key.VAR.^name` is `Str`. Routing it made the key track later writes — a divergence, not a
  fix. It keeps the snapshot producer, and both facts are now pinned.
- **`.kv` is deferred, for a reason in the *consumer*.** A `.kv` loop is a **multi-parameter** loop,
  and those bind through the bind-prefix `Stmt::Assign`s `build_for_bind_stmts` emits, each reading
  its chunk element through the ordinary element chokepoint — which **decontainerizes**. A cell handed
  out here arrives at `$v` as a plain value while the writeback that used to carry the mutation has
  been retired for that iteration, so routing `.kv` *lost* the direct write
  (`for @a.kv -> $i, $v is rw { $v += $i }`) instead of gaining the deferred one. It needs a raw bind
  for an rw scalar multi-parameter first — the shape `@`/`%`-sigil multi-params already have via
  `Stmt::MarkBind`. **That bind landed with ADR-0045 slice 5 on 2026-09-01 and `.kv` is routed now**;
  see [the news entry](../../news/2026-09/kv-hands-out-element-containers-to-a-multi-param-loop.md).

**Row 10 is deferred to a prerequisite, with a measured reason.** The fix itself is three lines —
compile a FatArrow's Index RHS in the container-producing mode (`scalar_bind_autovivify` +
`bind_terminal`) that the `=:=` and `return-rw` arms already use, next to the existing `WrapVarRef`
capture in `compiler/expr_binary.rs`. But **`array_slot_ref` grows the array at bind time where raku
defers until the write**: `my @a = 1,2; my $r := @a[5]; @a.elems` is `6` in mutsu and `2` in raku.
`key => @a[i]` is ordinary, common code, so routing it through the primitive would spread that eager
growth to every such pair. The hash side already has the deferred token (`hash_slot_ref` returns a
lazy `HashEntryRef` for a missing key); the array side needs the same. Recorded as
`todo/tickets/array-slot-ref-vivifies-eagerly-where-raku-defers.md` — a prerequisite for row 10
rather than part of it.

**Row 11 moves from slice 3 to slice 4.** Slice 3 does the half it owns: a `List` receiver keeps the
snapshot producer, so its pair value is a bare item with nothing to alias, which §2.2 says is the
whole of the immutability story. What still swallows `$l.pairs[0].value = 3` is the *other* half —
the env-scan compensator finds `$l`'s own list as a candidate container, rebuilds it, and reports
success. The read-only guard is only reachable once that scan is deleted, which is slice 4's job.

**§5's open questions, as measured by slice 3:**

- **Q1 (eager vs lazy promotion)** — eager, as recommended, and here is the cost. Release build, best
  of three on an otherwise-idle machine, a *read-only* pass over a 200 000-element array:
  `.values` 0.07 s → 0.18 s, `.reverse` 0.07 s → 0.18 s, `.sort` 0.05 s → 0.18 s. The two producers
  that are NOT routed are the controls and stay flat: `.pairs` 0.23 → 0.25 s, `.kv` 0.73 → 0.77 s.
  The repository's own benchmarks do not move
  (`bench-array`/`bench-hash`/`bench-string`/`bench-fib` unchanged, `bench-class` 0.15 → 0.14,
  `bench-ctor` 0.28 → 0.26), so this is a per-call cost on large containers rather than a corpus-wide
  one. Lazy promotion at reification stays the option if a bench CI series shows it.
- **Q4 (is `resolve_array_entry` the only read chokepoint?)** — **no, and this is the ADR's most
  important correction.** It is the only chokepoint for *element* reads, but bulk iteration
  (`h.iter()`, `items.iter()`) and `ValueView::Pair(k, v)` destructuring walk the storage directly.
  For a flat list of cells that does not matter — list consumers decontainerize — which is why
  `.values`/`.reverse`/`.sort` ship. For a Pair carrying a cell it matters a great deal, which is why
  `.pairs` does not (above).
- **Q5 (does anything depend on the pair being a snapshot?)** — **yes, and Q5 was right to single it
  out as the likeliest slice-3 regression.** The first one found was
  `"...".trans(%matcher.pairs)`, which asks what the pair *value* is (`is_closure`, then the
  Regex/Array/Range shape match); a `ContainerRef` answered "no" to all of them, silently turning a
  closure replacement into a stringified one. Four more followed. The lesson generalizes: the hazard
  is not *reading* a promoted value, it is **type-testing** one — and the pair wrapper is what
  carries it to code that does.
- A related pre-existing leak was fixed on the way in: `.WHAT` on a `ContainerRef` receiver answered
  `Scalar` instead of the value's type. Harmless while cells were rare; with slice 3 handing them out
  in bulk, `@a.pairs[0].value.WHAT` would have started answering `Scalar` where it answers `Int`
  today. `^name` still answers `Scalar`, because that is what `.VAR.^name` needs and mutsu cannot yet
  tell a cell reached *through* `.VAR` from a bare aliased value —
  `todo/tickets/var-on-a-containerref-is-not-distinguishable.md`.

## Implementation status — §1.3 row 10 (2026-08-28)

**Row 10 is green.** Its prerequisite landed first: `Value::array_slot_ref` no longer grows the array
at bind time. An index past the end now yields a deferred vivification token
(`EntryRoot::Array` + `EntryStep::Index`, the array twin of `hash_slot_ref`'s missing-key
`HashEntryRef`), and the first write through the binding walk-creates it, filling the gap with the
element hole value. `my @a = 1, 2; my $r := @a[5]; @a.elems` is `2` again, matching raku, and an
unwritten bind on a typed / `is default(...)` array reads that array's hole value (`Int`, `42`)
rather than a blanket `Any`.

With the eager growth gone, row 10 is the three-line compiler change slice 3 described: a FatArrow
whose RHS is an `Expr::Index` compiles in the container-producing mode (`scalar_bind_autovivify` +
`bind_terminal`) that `=:=` and `return-rw` already use, so the Pair's value *is* the element's
shared cell — no `WrapVarRef` boxing, which only ever applied to a bare `Expr::Var`. Both the
in-range row (`my $p = 0 => @a[0]; $p.value = "x"`) and its out-of-range companion
(`my $p = 'k' => @a[5]`, which must not grow `@a` until `.value` is written) are pinned in
`t/subscript-pair-element-container.t`, and the whole file passes under real `raku` too.

The Pair-value leak that backed `.pairs` out (above) does not bite here: the FatArrow arm promotes
**one** named element on demand, exactly like the `:p`/`:kv` adverbs of slice 2, rather than handing
a coercion a whole container's worth of cells.

**Still open in slice 3/4**: rows 3, 4, 9 (`.pairs` routing), row 11 (env-scan compensator) and
row 12 (element type constraint). One divergence is knowingly left behind by the array-token work: a
bound *slice* (`my @s := @a[1,5]`) and the two multi-dim descents still grow the array eagerly,
because their promoted cells are stored as elements of *another* array and an out-of-range index
would put a deferred token where neither `resolve_array_entry` nor the bound-slice write-through
recognizes one (measured: `roast/S32-array/multislice-6e.t`). Tracked in
`todo/tickets/bound-array-slice-still-vivifies-eagerly.md`.

---

## Implementation status — slice 4, the reporting half (2026-09-01)

The **enforcement** half of this slice landed with ADR-0059's `is rw` bare-tail work (#7190):
`array_slot_ref`/`hash_slot_ref` seed the promoted cell with the container's `value_type`, so a write
through any alias -- the `:=` bind, `%h<k>`, the `:p` pair value, ADR-0045's `for`-loop element alias,
the implicit topic -- is refused instead of silently landing. Row 12 of §1.3 is green.

What that left wrong was the **report**, and this slice finishes it. A cell knew its type but not its
origin, so every element failure fell through to `RuntimeError::typecheck_assignment(.., None)` and
came out as `Type check failed in assignment; expected Int but got Str ("s")` -- no symbol at all,
where raku says `Type check failed for an element of @a` and names the container rather than the
alias the write came through. `$!.expected` was also a `Str`, not the expected type object, so
`$!.expected === Int` failed.

`ContainerCell::constraint` went from `Option<String>` to `Option<Box<CellConstraint>>` over
`{ ty, element_of }` -- rakudo's single `$!descriptor` split into the half that decides legality and
the half that decides blame, because both are observable and they word their failures differently (a
typed *scalar*'s cell keeps "in assignment to `$x`"). Boxing makes the field 8 bytes instead of 24,
so the cell shrank, which matters because ADR-0045 slice 4 promotes eagerly.

The promotion primitives are `Value` methods and cannot see a variable name, so they seed
`element_of` with the bare sigil -- exactly what raku prints for an anonymous container. Two sites
that had already resolved the real name for their own routing retag the cell
(`crate::value::retag_element_owner`): the `for`-loop element alias, and the loop's
producer-carried-cell arm, which is what makes `for @a.values` / `for @a.reverse` blame `@a` even
though `vm_element_producers.rs` only ever saw a receiver value. `for @a.sort` (no source tag at
all), the `:=` bind and the subscript adverbs still report `@`/`%`; closing that needs the name to
travel *with the container*, which is
`todo/tickets/promoted-element-cell-does-not-know-its-container-name.md`.

**The compensator deletion (the rest of slice 4) is NOT done** -- `methods_mut_method_lvalue.rs`'s
env-scan and the `__mutsu_hash_ref` branch are untouched, and §1.3 row 11 is still `todo`-marked in
`t/subscript-pair-element-container.t`.

Verified with `make test`, a **full local `make roast`** (required by the "universal property of
values" rule, since this changes what is inside every promoted container), and the bundled-battery
gate.

## 5. Open questions (the forks for the deciders)

1. **Does `.pairs` promote eagerly over the whole array, or lazily per reified element?**
   `.pairs` returns a `Seq`; promoting all N slots when only the first is inspected is avoidable if
   promotion happens at reification. Interacts with [ADR-0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md).
   *Recommendation: start eager (simpler, matches the current `Value::seq(pairs)` shape), measure,
   and make it lazy only if a bench regression shows up.*

2. **Do mutable QuantHash weights stay on the `topic_source_var` arm, or become cells?**
   A `BagHash` weight is not a stored element container in mutsu's representation and `.value = 0`
   *removes* the key, so it is a different operation with different semantics. *Recommendation: keep
   the weight arm; it is not part of the scan being deleted and `t/for-pairs-value-quanthash-writeback.t`
   pins it.*

3. **Does the shaped-array multidim `:p` form convert in slice 2 or later?**
   It has its own scan (`:613-646`) keyed by index tuple and its own rebuild path
   (`multidim_assign_nested`). *Recommendation: convert it in slice 2 with the rest of the adverbs —
   leaving it behind means the scan cannot be deleted in slice 4.*

4. **Is `resolve_array_entry` genuinely the only read chokepoint?**
   Its doc claims so and the `:=` path corroborates it, but that path promotes far fewer slots than
   `.pairs` will. *Recommendation: slice 2 lands behind the §4.1 test set; any leaked `ContainerRef`
   surfaces there as a wrong `.raku`/`.WHAT`/`.gist`, which is a deterministic failure, not a flake.*

5. **Does anything depend on the pair being a snapshot?**
   `.pairs` output feeding a `Hash` constructor, `is-deeply` comparison, or serialization must see
   values, not cells. Covered by the decontainerizing chokepoint if Q4 holds; called out separately
   because it is the most likely source of a slice-3 regression.

---

## 6. Consequences

- **`methods_mut_method_lvalue.rs` loses ~120 lines of search-and-rebuild**, and the surviving
  `method == "value"` arm becomes a short list of container kinds rather than a search.
- **`X::Assignment::RO: cannot assign through .value on non-instance` stops being reachable from
  correct programs.** It is currently the observable face of a failed search, which is why it reads
  as a nonsense diagnostic in the §1.3 rows.
- **More array/hash slots hold `ContainerRef`** during a program's life. This is the change with real
  blast radius: any consumer that pattern-matches on `ValueView::Array(arr, _)` and inspects
  `arr[i]` without going through `resolve_array_entry` will now sometimes see a cell. Q4/Q5 exist to
  bound it, and the failures are deterministic.
- **Element type constraints start being enforced** on element-container writes, including for
  `:=`-bound elements that bypass them today. This can surface as *new* deterministic failures in
  code that was silently storing the wrong type — that is the fix working.
- **`todo/deep/subscript-p-pair-is-a-snapshot-not-a-container.md` is superseded by this ADR** and
  should be removed when slice 2 lands, with the outcome recorded here and in `news/`.
- **If rejected / indefinitely deferred:** the twelve divergences in §1.3 stay, and the ambiguity
  class keeps being re-discovered as an unrelated-looking bug — a `my @b = @a;` added anywhere in
  scope can break a `.value =` elsewhere in the file, which is close to undebuggable from the error
  message alone. That is the specific cost of leaving it.

---

## 7. Adjacent open findings — the same model, different surfaces

"An array/hash element is a `Scalar` container" is a single Raku model that mutsu approximates in
three separate places. This ADR takes the *pair-producer* surface only, deliberately; the other two
are recorded here so a future reader can see the shape of the whole and not re-derive it.

- **`todo/deep/element-itemization-lost-in-scalar-binding.md`** — the *read* surface, now designed as
  **[ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md)** (2026-08-20; itemize at the
  element store). Because elements
  are containers, a bare element read is itemized: raku prints `$["a", "b"]` for `@d[0].raku` where
  mutsu prints `["a", "b"]`. Its bind-side half shipped (`news/2026-08/param-bind-itemization.md`);
  the store-side half is open. (ADR-0040 §1.4 retires this ADR's "survey-sized" sizing by
  measurement, and §5.3 records the two places the two ADRs meet: `array_slot_ref`/`hash_slot_ref`
  must not de-itemize what they wrap, and `hash_autovivify_cell`'s
  return-the-element-as-is arm.) **This ADR does not depend on it and does not advance it**:
  promoting a slot to a `ContainerRef` on demand
  is invisible to `.raku` (verified — `my $r := @a[0]; say @a.raku` is `["A", "B"]` in both), because
  the read chokepoint decontainerizes. Itemization is a separate question about what the
  decontainerized value *is*.
- **`todo/deep/for-loop-rw-element-alias-lost-through-deferred-closure.md`** — the *binding-lifetime*
  surface. `for @a -> $v is rw` binds a plain clone and snapshots it back once per iteration, so a
  closure that escapes the loop and mutates `$v` later writes a disconnected cell. That finding names
  the same missing primitive from the other side and reaches the same conclusion ("a genuine
  per-element `ContainerRef` alias"), and it is the natural *next* consumer of this ADR's routing:
  bind the loop param to `array_slot_ref(i, true)` and `write_back_for_rw_param` becomes unnecessary.
  One caveat it raises is worth answering here: it worries that an element store write-throughs *any*
  `ContainerRef` element unconditionally on reassign, with no replace-vs-alias distinction. For this
  ADR that behaviour is not a problem but the requirement — raku's `@a[0] = "Q"` assigns *into* the
  element's `Scalar`, never replaces it, which is exactly why `my $p = @a[0]:p; @a[0] = "Q"` must
  make `$p.value` read `Q`.

---

*Slices 1-3 of this ADR are implemented; the decision stands. If the mechanism judgment changes
later, supersede it rather than rewriting it.*
