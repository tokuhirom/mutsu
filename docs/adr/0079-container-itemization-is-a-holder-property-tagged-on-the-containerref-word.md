# ADR-0079: Container itemization is a property of the *holder*, tagged on the `ContainerRef` word

- **Status**: Proposed
- **Date**: 2026-09-09
- **Deciders**: tokuhirom, Claude
- **Related**: [#7542](https://github.com/tokuhirom/mutsu/issues/7542) (the originating finding, whose
  proposed fix this ADR rejects on measurement),
  [ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md) (elements are itemized at the
  store — the rule this ADR extends from element *values* to the container *word*),
  [ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md) (the aliasing surface of the
  same Raku model), [ADR-0057](0057-var-reflection-identity-cell-address.md) (`.VAR` identity is the
  cell address — the invariant that makes a per-word tag free),
  [`docs/scalar-array-sharing.md`](../scalar-array-sharing.md) (slice 2a/2b, the share path this ADR
  changes)

> `my $hi = %h` makes `$hi` a `Scalar` container holding the hash, so `$hi.raku` is `${…}` and
> `my %c = ($hi,)` dies. mutsu gets both wrong, because the share path puts the **same
> `ContainerRef` word** in `$hi`'s slot and in `%h`'s slot — leaving nowhere to record that only one
> of the two holders is itemized. This ADR decides to put that bit on the `ContainerRef` *word*
> (a second nanbox `Kind`), which is where mutsu already puts the same bit for `Hash` and `Array`.

---

## 1. Context

### 1.1 What #7542 claimed, and why it is not what is happening

The issue reports that two shapes collide — a `$`-itemized hash and a destructuring staging temp's
element container both spell as `ContainerRef(<Hash>)` — and proposes to fix it by moving the
itemization *into the cell's contents*, so every value consumer can decontainerize unconditionally.

Measured against rakudo (v2026.07), **the two shapes are not two shapes**. raku treats an
`Array` element exactly as it treats a `$` scalar, and the thing that must not itemize is a `List`
element, which is not a container at all:

```raku
my %h = a => 1, b => 2;

my @a; @a[0] = %h;
  @a[0].VAR.^name  # Scalar      @a[0].raku  # ${:a(1), :b(2)}   →  %c = (@a[0],) dies
my @l := 1, %h;
  @l[1].VAR.^name  # Hash        @l[1].raku  # {:a(1), :b(2)}    →  flattens
```

So `ContainerRef(<Hash>)` should **always** decontainerize to an *itemized* hash. The destructuring
staging temp (`@__destructure_tmp__`) is right to be excluded from element-container promotion, but
not for the reason its comment gives: it is excluded because **it models the RHS `List`, and a
`List`'s elements are not containers** — the same reason ADR-0040 slice 2 excludes it from element
itemization. That is a statement about what the temp *is*, not an admission that mutsu cannot tell
two things apart.

### 1.2 Why the issue's proposed fix cannot work

`Interpreter::array_share_assign` (`src/vm/vm_var_assign_coerce.rs:789`) installs **one and the same
`Value` word** in both slots:

```rust
let container = Value::container_ref(cell);
self.locals[source_idx] = container.clone();   // %h
…
self.locals[idx]        = container.clone();   // $hi
```

Any bit stored *inside* the cell — or on the cell — is therefore read by both holders, and
`%h.raku` would become `${…}`, breaking the one row mutsu currently gets right. Itemization is a
**per-holder** property (the doc comment on `Value::hash_is_itemized`,
`src/value/value_methods_a.rs:376`, already says so: "two holders of the same hash data can differ in
itemization"), and a shared cell has more than one holder. The bit has to live on something each
holder owns a copy of, and the only such thing is the `Value` word itself.

### 1.3 The divergence, measured on `main` (987ccfb, 2026-09-09)

```raku
my %h = a => 1, b => 2;   my $hi = %h;   my @arr = 1, 2;   my @a = 1, %h;
```

| # | program | raku | mutsu | |
|---|---|---|---|---|
| 1 | `$hi.raku` | `${:a(1), :b(2)}` | `{:a(1), :b(2)}` | ❌ |
| 2 | `my ($y, %r) = 2, $hi` | dies `X::Hash::Store::OddNumber` | succeeds | ❌ |
| 3 | `my $n = @arr; $n.raku` | `$[1, 2]` | `[1, 2]` | ❌ |
| 4 | `sub f($x) {$x.raku}; f(%h)` | `${:a(1), :b(2)}` | `{:a(1), :b(2)}` | ❌ |
| 5 | `my ($y, %r) = @a` | dies `X::Hash::Store::OddNumber` | succeeds | ❌ |
| 6 | `my ($y, %r) = @a[0], @a[1]` | dies `X::Hash::Store::OddNumber` | succeeds | ❌ |
| 7 | `my ($y, %r) = 2, %h` | succeeds (flattens) | succeeds | ✅ |
| 8 | `%h.raku` — **the invariant** | `{:a(1), :b(2)}` | same | ✅ |
| 9 | `%h.item.raku` | `${:a(1), :b(2)}` | same | ✅ |

Rows 1–4 are this ADR's scope: they are all one bug — the share path has nowhere to record the
holder's itemization, so it records nothing. Row 9 is the contrast that proves the *value*-level
machinery is already complete: `.item` produces a bare (unshared) itemized hash and everything
downstream honours it. Rows 5–6 have a **different** cause and are a follow-up (§6).

### 1.4 The mechanism already exists, twice

mutsu already carries "this holder is itemized" as a per-word tag, for exactly the two aggregate
kinds that must not leak a `Scalar` wrapper into value operations (ADR-0040 §1.4):

| held type | how the holder's itemization is spelled |
| --- | --- |
| Array / List | the `ArrayKind` tag — `Kind::ArrayArray` vs `Kind::ArrayItemArray` |
| Hash | the kind tag — `Kind::HashPlain` vs `Kind::HashItemized` |
| everything else | a `ValueRepr::Scalar` wrapper |

`ContainerRef` is the one holder shape with no such flavour, and it is the one shape that can be
shared between a `$` holder and a `%`/`@` holder. That is the whole gap.

### 1.5 Everything cell-side is keyed on the cell, not on the word

The argument that this is cheap: every piece of state hanging off a container is addressed by the
`Gc<ContainerCell>` (its fields, or its pointer address as a map key) — the `of`-type constraint
(`register_container_constraint`, `src/value/mod.rs:579`), the frozen set
(`FROZEN_CONTAINER_CELLS`, `src/value/mod.rs:652`), the QuantHash writeback
(`quanthash_weight_ref`, `src/value/mod.rs:572`), and `.VAR` identity (ADR-0057, the cell address).
Two differently-tagged words pointing at one cell agree on all of it. A per-word tag adds a
distinction *only* where raku has one.

---

## 2. Decision

**Itemization of a shared container is a property of the holder, and is carried on the
`ContainerRef` word.**

Concretely:

1. Add a second nanbox kind, `Kind::ContainerRefItemized`, beside `Kind::ContainerRef`. Both carry
   the same `Gc<ContainerCell>` payload and both decode to `ValueRepr::ContainerRef` / view as
   `ValueView::ContainerRef`. The distinction is visible only through a new probe,
   `Value::container_ref_is_itemized()`, and a new constructor,
   `Value::container_ref_itemized(cell)`.
2. **Plain is the default.** Every existing `Value::container_ref` call site keeps producing the
   plain flavour; sites opt in one at a time, each with its own raku measurement. This is what keeps
   the blast radius small — see §5.
3. Where a `ContainerRef` is unwrapped into a **value**, an itemized word yields an itemized value
   (`Value::itemize_for_element_store`, `src/value/value_methods_a.rs:437`); a plain word yields the
   contents unchanged. Value consumers therefore decontainerize **unconditionally** — the rule the
   rest of the element-container work is built on — and the `unwrap_contained_pair`
   (`src/builtins/map_hash_coerce.rs:51`) "only unwrap Pairs" hedge can go.
4. The `$`-share target (`$hi` in `my $hi = %h`) gets the itemized flavour; the share **source**
   (`%h`, `@z`) keeps the plain one. This is the split that #7542's proposal could not express.
5. The destructuring staging temp stays excluded from element-container promotion
   (`Interpreter::promotable_array_len`, `src/vm/vm_element_producers.rs:222`) and from element
   itemization (`deitemize_real_array_elements`, `src/runtime/utils/coerce_containers.rs:81`), with
   its justification rewritten: **the temp models a `List`, and a `List`'s elements are not
   containers** (§1.1). It is not a stopgap for an ambiguity, because there is no ambiguity.

---

## 3. Options considered

### A. Put the itemization inside the cell's contents (#7542's proposal) — **rejected**

The share path installs one word in both slots (§1.2), so the flag is read by the `%`/`@` holder
too and `%h.raku` becomes `${…}` (row 8). It can only be rescued by de-itemizing again at the
`%`/`@` read — reinstating exactly the "compensate at the read" pattern ADR-0040 rejected, and
reinstating it at a site (`GetLocal`) that is *not* the only reader of the slot (env sync, closure
capture, saved call frames, `.VAR`).

### B. Tag the `ContainerRef` word — **chosen**

- Third instance of a pattern mutsu already runs twice (§1.4), so no new representational idea.
- `ValueView::ContainerRef` stays a single variant, so the **410** `ValueView::ContainerRef` /
  `is_container_ref()` read sites are untouched.
- Plain-by-default keeps the **67** `Value::container_ref(` construction sites untouched; the opt-in
  set is a handful.
- Cell-side state is keyed on the cell, so nothing else in the container machinery notices (§1.5).
- Fixes rows 1–4 together, Array and Hash alike, because the tag is on the wrapper rather than on
  the held type.
- Costs: one nanbox kind (70 of 104 used — `KIND_COUNT`, `src/value/nanbox/mod.rs:185`), and the
  unwrap chokepoints have to learn the tag. The second is the real risk and is what §5 addresses.

### C. Compensate at the read from the variable's sigil — **rejected as a general answer**

Already in use for one method: `.VAR` on a shared container reaches for the *source variable name*
(`src/runtime/methods_call_dispatch.rs:4164`), which is why row 8's `%h.VAR.^name` is `Hash` and
`$hi.VAR.^name` is `Scalar` today. It is cheap and needs no representation change, but it does not
generalize: the consumption site of `my %c = ($hi,)` has no name to consult, and neither does a
return value, a closure capture, or an element. Rows 2, 5 and 6 stay broken. Keeping it for `.VAR`
is fine; growing it is how mutsu accumulates the compensators ADR-0040 §1.5 catalogued.

### D. Keep the status quo — **rejected**

Defensible in that the existing exclusion is, as measured, the *correct* rule. But rows 1–4 are
user-visible raku divergences with a single shared cause, and the representation gap means the wall
is hit again by any future producer that hands a `Hash`-valued container into a hash initializer.

---

## 4. Implementation slices

**Slice 0 — retire the wrong justification (comments only, no behaviour).** Rewrite the comment in
`promotable_array_len` (`src/vm/vm_element_producers.rs:233-252`) and in
`deitemize_real_array_elements` (`src/runtime/utils/coerce_containers.rs:76-81`) to state the List
reason from §1.1 and cite this ADR. Nothing else changes; the exclusion stays.

**Slice 1 — representation.** Give the repr a flavour, exactly as `Hash` has one:
`ValueRepr::ContainerRef(Gc<ContainerCell>, bool)` mirroring
`ValueRepr::Hash(Gc<HashData>, bool)`. There are only **six** `ValueRepr::ContainerRef` match sites
in the tree, and the 22 `Value::ContainerRef(cell)` shorthand call sites go through one shim
(`src/value/mod.rs:1839`), so they are covered by keeping that shim's meaning as "plain" and adding
a second one. Then add `Kind::ContainerRefItemized` and thread it through every place
`Kind::ContainerRef` is enumerated:

| site | what it does | consequence of forgetting it |
| --- | --- | --- |
| `encode.rs:220-226` | packs the word — **and latches `note_container_cell()`**, the zero-counter proof the JIT's Tier B inline `GetLocal` relies on | a cell exists that the JIT believes cannot |
| `decode.rs:191` | unpacks to the repr | wrong flavour on round-trip |
| `peek.rs:314` | `is_container_ref()` | the 410 read sites stop seeing itemized containers |
| `peek.rs:396-413` | `may_hide_a_stringifiable_list` — a tag probe on every `~`/`eq` operand | `~`/interpolation silently skips the deref |
| `peek.rs:654` | `ValueView::ContainerRef` projection | the view splits in two, which this ADR forbids |
| `mod.rs:441` | the `gc_op::<ContainerCell>` payload/trace arm | GC does not trace the cell — a use-after-free class bug |

Add `Value::container_ref_itemized()` and `Value::container_ref_is_itemized()`, and extend the
exhaustive kind round-trip in `src/value/nanbox/tests.rs`. **Nothing constructs the new kind yet**,
so this slice is behaviour-neutral and its whole job is to keep the representation change separate
from the semantic one.

**Slice 2 — the `$`-share holder (rows 1, 3, 4).** In `array_share_assign`
(`src/vm/vm_var_assign_coerce.rs:789-861`), build **two** words over the one cell: the plain flavour
for the source slot / env alias / saved frames, the itemized flavour for the target slot, its env
alias and the `__mutsu_array_share::` bookkeeping. Then teach the unwrap chokepoints —
`Value::into_deref` / `deref_container` / `with_deref` (`src/value/value_methods_a.rs:537-580`),
`Interpreter::resolve_array_entry` (`src/vm/vm_var_ops.rs:119`) and `unwrap_contained_pair` — to
apply `Value::itemize_for_element_store` when the word was itemized. Row 4 (`f(%h)` binding a `$`
parameter) rides along if the parameter bind shares rather than copies; measure it, and if it takes
the copy path it belongs to `itemize_plain_scalar_param` instead. Pins: extend
`t/hash-itemization-flag.t` and `t/scalar-array-share.t` with rows 1–4 and, as a guard, row 8.

**Slice 3 — audit the remaining `Scalar`-container producers.** Decide, one site at a time and each
against a raku measurement, which of the 67 `Value::container_ref` sites model a raku `Scalar`
container and should therefore opt in. The known candidates are the element cells from
`array_slot_ref` / `hash_slot_ref` (`@a[0].VAR.^name` is `Scalar` in raku) and `WrapVarRef` for a
`$`-sigiled `is rw` argument. **Expect most of them to need no change**: ADR-0040 already itemizes
the element *value* at the store, which is why `@a[0].raku` is `${…}` in mutsu today. Where both
mechanisms would apply, prefer the word tag and leave the stored value bare — one holder property,
one spelling.

**Slice 4 — drop the hedge.** With the tag in place, `unwrap_contained_pair`'s "unwrap only if the
contents are a `Pair`" special case is no longer load-bearing; make the value-context unwrap
unconditional and delete it.

---

## 5. Consequences and risks

- **The chokepoint change in slice 2 is the risk, and it is deliberate.** Making a deref itemize is
  a change to a path every value read passes through. It is guarded by the plain-by-default rule
  (only words the opt-in sites built can itemize), by `t/` and by the full roast run. Per the
  repo's "refactor boldly" rule, a red CI on the branch is the safety net doing its job, not a
  reason to shrink the slice.
- **Row 8 is the invariant to protect.** `%h.raku` must stay `{…}` and `%h.VAR.^name` must stay
  `Hash` after a share. Every slice carries that as a test.
- **No perf cost.** The tag rides in the existing kind byte; no allocation, no extra indirection,
  no change to the cell. The one thing to get right is that the new kind latches
  `note_container_cell()` like the old one (slice 1 table): the JIT's Tier B inline `GetLocal` fast
  path is gated on that counter being zero, so a kind that skips the latch would let the JIT elide
  a deref that is now needed.
- **`.VAR`'s name-based compensation (§3 C) becomes redundant** for the shared-container case once
  slice 2 lands. Removing it is optional cleanup, not part of this decision.
- **`Kind` budget**: 70 → 71 of 104. Not a constraint, but worth noting that this pattern (a second
  kind per holder flavour) does not scale indefinitely.

## 6. Follow-ups, explicitly out of scope

- **Rows 5–6** (`my ($y, %r) = @a` should die) have a different cause: ADR-0040 slice 2's
  `deitemize_real_array_elements` strips element itemization from the staging temp *unconditionally*,
  where the correct rule is that the temp neither **adds** itemization nor **removes** what its
  source produced (raku: a `List` literal's bare `%h` flattens, but an `Array`'s element does not).
  Fixing it needs an ADR-0040 addendum and touches destructuring generally. File as its own issue
  once slice 2 lands, so the two changes are not diagnosed through each other.
- **`ContainerView`** (`Kind::ContainerView`) was not examined. If it turns out to model a holder
  too, it wants the same treatment; nothing in this ADR assumes either way.

## 7. Implementation status

| Slice | State |
| --- | --- |
| 0 — comment justification | Complete (2026-09-09; comments now state the RHS `List` rule) |
| 1 — `Kind::ContainerRefItemized` | Complete (2026-09-09; the holder flavour is encoded, decoded, probed, projected through the unchanged `ValueView::ContainerRef`, traced by GC, and covered by NanBox round-trip tests) |
| 2 — `$`-share holder (rows 1, 3, 4) | Complete (2026-09-09; scalar shares and scalar parameter binds now carry itemization on the target word, while source words remain plain; dereference and hash-initializer consumers preserve the distinction) |
| 3 — audit remaining producers | Not started |
| 4 — drop the `unwrap_contained_pair` hedge | Not started |
