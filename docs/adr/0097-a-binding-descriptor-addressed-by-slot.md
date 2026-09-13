# ADR-0097: A binding's own metadata lives on a slot-addressed descriptor, not under a key derived from its name

- Status: Proposed
- Date: 2026-09-13
- Related: [ADR-0042](0042-type-constraints-belong-to-the-container-not-to-a-name.md)
  (a type constraint belongs to the container, not to a name — the same
  principle, applied to one property and already implemented),
  [ADR-0084](0084-the-frame-env-is-not-the-programs-symbol-table.md) (the
  complementary axis: entries that are not lexical variables at all leave the
  frame `Env`), [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md)
  (slot-addressed lexical capture), [ADR-0039](0039-container-lexicals-resolve-lexically.md)
  (by-name lookup replaced by an owning lexical scope), and
  [ADR-0064](0064-var-descriptor-carries-the-contained-value.md) (the `.VAR`
  descriptor)
- Addresses: [#8069](https://github.com/tokuhirom/mutsu/issues/8069) §4.1 and
  [#8087](https://github.com/tokuhirom/mutsu/issues/8087) stage 4, which are the
  same work seen from two sides

## 1. Context

### 1.1 The shape of the problem

`my Int @a[4;4]` declares one binding with a handful of settled properties: it
is an array, its elements are `Int`, it has declared dimensions, it is not
`:=`-bound, it is not readonly, it has no `is default`, it is not a sigilless
alias for something else. Every one of those is fixed at the declaration and
cannot change for the life of the binding.

mutsu stores them as **sibling entries in the same string-keyed `Env` as the
variable itself**, under keys built from the variable's own name —
`__mutsu_type::@a`, `__mutsu_shaped_array_dims::@a`, `__mutsu_bound::@a`,
`__mutsu_sigilless_alias::@a`. There is no edge from the binding to its own
metadata, so the only way to ask a question about a binding is to **rebuild the
key from its name and probe the map again**.

That is why the same finding keeps coming back. Five separate perf campaigns
have each profiled a `format!`-built key on a hot path and each fixed the sites
its own profile happened to walk through (#7571, #7766, two `bench-ctor` rounds,
and #8069's element store); #8087 is the issue opened when the pattern was
noticed to be recurrent rather than incidental.

### 1.2 What has already been done, and why it is not the fix

#8087 stages 1-3 are merged. Every one of the **48** `__mutsu_*` namespaces now
goes through `MetaNs` (`src/runtime/meta_ns.rs`), a memoizing constructor with
no public string form, and `scripts/check-magic-keys.sh` fails the build on a
hand-built key. That removed the `format!` and the repeated `Symbol::intern`.

**It did not remove the probe.** A memoized key is still a key: the store path
still hashes it into a map to discover a fact that was settled at the
declaration. Stage 3's own measurement is the evidence — flat on every
benchmark, because most of what it converted is latch-gated and never runs at
all in a program that does not use the feature. The cost that remains is not
key construction.

Of those 48 namespaces, **26 are keyed by a single lexical binding's name** and
are therefore in this ADR's scope:

```
sigilless_alias  sigilless_readonly  type       hash_key_type   state_key
bound            bound_index         shaped_array_dims          atomic_arr
atomic_hash      array_share         atomic_name                bound_array_len
bound_array_slice                    bound_decont               constant_var
deep_readonly    deleted_index       elem_share                 gather_self_ref
scalar_bind_no_container             ro_index                   shared_dirty
var_meta         var_source_name     outer
```

The other 22 are keyed by a routine, a package, a compunit, a role, an attribute
or a per-execution identity. Those are **not** this ADR's business: the
routine/package/compunit ones belong to ADR-0084's axis, and the role/attribute
ones are mixin-registry keys in a `String`-keyed `MixinOverrides` map, not env
keys at all.

### 1.3 The descriptor already exists — in twelve pieces, none of them named

This is the finding that motivates writing the decision down now. `CompiledCode`
carries **twelve** parallel per-slot / per-name side fields, each added by a
different campaign, each a partial answer to the same question:

| field | what it holds | what it saves |
| --- | --- | --- |
| `locals_alias_sym` | the `__mutsu_sigilless_alias::<name>` key symbol | the `format!` + intern |
| `locals_readonly_sym` | the `__mutsu_sigilless_readonly::<name>` key symbol | ditto |
| `locals_deleted_index_sym` | the `__mutsu_deleted_index::<name>` key symbol | ditto |
| `locals_bound_slice_sym` | the `__mutsu_bound_array_slice::<name>` key symbol | ditto |
| `locals_scalar_no_container_sym` | the `__mutsu_scalar_bind_no_container::<name>` key symbol | ditto |
| `plain_locals` | *is this slot's name a plain lexical* | the probe as well |
| `simple_scalar_locals` | *can a store into this slot skip the whole cascade* | the probe as well |
| `state_locals` | slot → `state` key | a name lookup |
| `our_locals` | slot → qualified package name | a name lookup |
| `scalar_bind_locals` | the slots declared `my $x := …` | a name lookup |
| `param_local_slots` / `param_locals` | parameter ↔ slot | a name search |

Read that table as a whole and the design is already decided in practice: **a
binding's settled facts are being hoisted, one at a time, onto structures
indexed by the binding's slot.** Five of them stop at the key symbol (cheaper
probe, same probe); two of them — `plain_locals` and `simple_scalar_locals` —
go the whole way and *are* §4.1's `flags`, for two specific questions, under two
ad-hoc names.

What is missing is not the mechanism. It is a name for the mechanism, one place
to put the next property, and a rule that stops the next campaign from adding a
thirteenth vector.

### 1.4 What it costs today

#8069's element store is the best-measured instance. After #8107 and #8151 the
single-threaded in-range `@a[$i] = $v` is at **1,210 instructions, 0.02 heap
allocations and 277 ns**, against rakudo's ~40 ns and §5's ≤150 ns bar. A
per-line accounting of the residual (recorded on #8069 by
`claude/vibrant-dijkstra-e6djrd`) found no stray probes left — every line is
either a real lookup, a `Value::view()` decode, or a correctness check whose
comment justifies it by name:

| cost | Ir/call | removed by a descriptor? |
| --- | ---: | --- |
| `env().get_sym(var_sym)` — the container lookup | 47 | **yes** — the descriptor holds the handle |
| `resolve_local_slot` + `locals[slot].view()` — dual-store coherence | 61 | **yes** — one store, no coherence question |
| five `Value::view()` decodes | ~110 | **partly** — `flags` already classifies what three of them re-derive |
| `itemize_value` ×2 | 44 | no — real Raku semantics |
| `drop_in_place::<Gc<ArrayData>>` | 34 | **yes** — no clone-then-drop of a handle the descriptor owns |
| `unit_lexical_slot` + `our_package_container_key` + `unit_lexical_container_cell` | 47 | **yes** — see §3.2 |
| everything else (guards, `is_readonly_sym`, `var_default`, `var_type_constraint_sym`, kind/bounds matches) | ~110 | **mostly** — these are the name-keyed probes |

So roughly 300-400 of the 1,210 instructions are the thing this ADR removes, and
the rest of the gap to rakudo is `Value::view()` decoding and genuine
itemization. **This ADR is not a plan to reach 40 ns**; see §7.

One caveat on the 277 ns, confirmed while writing this: the figure is a
*subtraction* (the same loop with and without the store), and both sides keep
moving. Re-measured on today's `main` (`14b6d3d1`), the 400,000-store loop is
317-355 ms across two runs **of the same binary minutes apart**, and the derived
marginal reads anywhere from 206 ns to 353 ns — a 1.7x spread with no code
change at all. That is why §6's acceptance criterion is `scripts/bench-det.sh`'s
deterministic instruction count and not wall clock: on this path a wall-clock
number cannot resolve the effect a slice of this ADR would have, and #8087 stage
3 already reported the same conclusion from the other direction.

### 1.5 The correctness half

The performance argument is the weaker of the two. #8107 shipped a fast lane
that read the container out of `env` directly, and that turned out to be wrong
twice — both times caught by an existing test rather than by reasoning:

- A module routine's own `our @arr` resolves through a **precedence order** —
  captured unit lexical, then the running routine's package `our` mirror, then
  `env` — because the bare env key belongs to whatever scope *loaded* the
  module. Reading `env` by name wrote the loading script's same-named array
  (`t/modules/our-container-bare-name-resolution.t`).
- Once a second mutator thread exists, the store belongs to the name-keyed
  cross-thread lanes or to ADR-0068's `ContainerStructGuard`; an unguarded
  `gc_contents_mut` under twenty concurrent writers is a `double free or
  corruption (out)` (`t/concurrency/concurrent-lane-decline-routes.t`).

Both are the same defect in different clothes: **a name is not an address.**
Resolving one requires knowing which scope owns it, and every consumer that
re-resolves a name has to re-derive that ownership — or, as happened here, quietly
fail to. A slot is an address; it *is* the resolution, already performed. The
bug class is not fixed by this ADR, it becomes unrepresentable.

## 2. Decision

**Proposed.** A binding's own metadata lives on a **descriptor addressed by the
binding's slot**, not under an `Env` key derived from its name.

The descriptor has two halves, split by when the fact is known:

1. **Compile-time half, on `CompiledCode`, one entry per local slot.** The
   properties a declaration settles: sigil and container kind, declared type and
   key type, declared shape, `constant`, `state`, `our`, sigilless, parameter,
   `:=`-bound-at-declaration. This subsumes the twelve fields of §1.3 into one
   `Vec<BindingDesc>`, and the `flags: u32` of #8069 §4.1 is its bitfield —
   `plain_locals` and `simple_scalar_locals` become two of its bits rather than
   two more vectors.

2. **Runtime half, per frame, parallel to `locals`.** The facts a *running*
   program discovers and can change: a `:=` rebind, a bound or `=`-shared
   element, a `:delete`d index, a readonly-by-alias parameter, a cross-thread
   lane. These are the ones that cannot be baked, and they are also the ones
   currently behind the `env::*_possible()` latches — so the runtime half starts
   empty and stays empty for a program that uses none of those features.

**The invariant this establishes, and the reason it needs an ADR rather than a
PR description:**

> **No store-time or read-time probe may be keyed by a variable's name.**

§4.1 of #8069 states the reason plainly and it is worth repeating: without an
enforced invariant the probes grow back one feature at a time, which is exactly
how 26 namespaces and twelve side vectors accumulated. `MetaNs` plus
`check-magic-keys.sh` already enforce *"there is one way to build such a key"*;
this ADR's endgame is *"there is no such key"*, and the gate should tighten to
match as each namespace retires.

## 3. What makes this hard, and how it is answered

### 3.1 A slot is per-frame; a binding may outlive its frame

Closures capture, `our` publishes into a package, `state` persists across calls,
a unit lexical belongs to a compunit. A descriptor cannot simply be
`frame.descs[slot]` for those.

**Answer:** the descriptor belongs to the *binding*, and the slot is how the
currently-running frame reaches it. Where a binding already has a home that
outlives the frame — a `ContainerCell`, a `unit_lexicals` entry, a package
mirror — the descriptor travels with that home, which is the route ADR-0042
already took for the type constraint (embedded in `ArrayData`/`HashData` and on
the `ContainerCell`, with the `var_type_constraints` map deleted). The frame's
per-slot vector is a cache of the resolution, not the owner.

### 3.2 The three-way resolution precedence is the crux

`env_root_descended_mut_tracked` resolves a write in a strict order: captured
unit lexical → the running routine's package `our` mirror → `env`. Any
slot-addressed scheme has to reproduce that, or reproduce #8107's bug.

**Answer, and this is the central design claim:** it does not have to reproduce
it — it has to *record its outcome*. The precedence is a property of the
declaration site, not of the store, so it is resolved once when the descriptor
is built and the descriptor then names the owning scope directly. The three
probes in §1.4's table (47 Ir per store, on every store, in every program)
become zero, and the failure mode where a consumer forgets one of them stops
existing. This is the single strongest argument for the whole change, and it is
a correctness argument, not a perf one.

### 3.3 The dual store

`locals` and `env` are two stores for one set of bindings, and the element-store
lane spends 61 Ir per store checking they agree. The descriptor does not fix the
dual store — that is ADR-0084's and #7817's work — but it must not entrench it:
the descriptor is attached to the binding, so it has exactly one copy regardless
of how many stores mirror the *value*.

## 4. Invariants that must continue to hold

- **Lexical shadowing.** An inner `my` shadows an outer binding of the same name,
  and a block's declarations do not outlive it — including two same-named slots
  in one chunk (`code.locals == ["%h", "%h"]`), which is why the compiler-baked
  slot exists at all.
- **`our` / unit-lexical precedence.** §3.2, and
  `t/modules/our-container-bare-name-resolution.t` is the pin.
- **Cross-thread routing.** A store in a program with a live second mutator
  thread still reaches the name-keyed lanes or `ContainerStructGuard`
  (ADR-0068); `t/concurrency/concurrent-lane-decline-routes.t` is the pin.
- **Closure capture.** A closure sees the bindings in scope at creation,
  including later mutations through shared cells (ADR-0025, ADR-0039). A
  descriptor must not turn a shared cell into a snapshot.
- **`EVAL` and re-entrant compilation.** A slot index is only meaningful against
  the `CompiledCode` that minted it; an `EVAL`'d chunk has its own.

## 5. Slices

Each slice retires one namespace family and deletes its `MetaNs` variant, so the
`check-magic-keys` surface shrinks monotonically and any slice is independently
revertible.

1. **Name the structure.** Introduce `BindingDesc` and the compile-time
   `Vec<BindingDesc>` on `CompiledCode`, and fold in the two flags that are
   already answers (`plain_locals`, `simple_scalar_locals`) plus the five
   key-symbol vectors. No behaviour change, no namespace retired — this is the
   consolidation that makes every later slice a one-site change.
2. **The declaration-settled properties**: `type`, `hash_key_type`,
   `shaped_array_dims`, `constant_var`, `deep_readonly`. All are fixed at the
   declaration, and the first two already have an authoritative home on the
   container: ADR-0042 slice 3 deleted the `var_type_constraints` side table, but
   the `__mutsu_type::<name>` env key outlived it and `var_type_constraint_sym`
   still probes it. So this slice mostly *deletes a second source of truth* that
   ADR-0042 left behind rather than inventing a new home.
3. **The binding-shape properties**: `sigilless_alias`, `sigilless_readonly`,
   `scalar_bind_no_container`, `bound`, `bound_decont`, `var_source_name`,
   `outer`. These are what the scalar store cascade probes on every assignment.
4. **The element-state properties**: `bound_index`, `deleted_index`,
   `elem_share`, `ro_index`, `bound_array_len`, `bound_array_slice`. Runtime
   half; all latch-gated today, so the win is the deletion of the latches rather
   than a measured speedup.
5. **The cross-thread lane keys**: `atomic_arr`, `atomic_hash`, `atomic_name`,
   `array_share`, `shared_dirty`. **Do not start this before #8069 §4.2**, which
   is the decision about whether the name-keyed lane survives at all; retiring
   its keys onto a descriptor while the lane itself is being replaced would be
   work done twice.

Slices 1-3 are the ones that pay; 4 and 5 are bookkeeping that follows.

## 6. Acceptance

- The `check-magic-keys` surface falls from 26 name-keyed namespaces toward
  zero, one slice at a time, and the gate tightens to forbid re-adding one.
- `@a[$i] = $v` loses the ~300-400 Ir §1.4 attributes to name-keyed probes and
  container re-lookup — measured with `scripts/bench-det.sh`, not wall clock,
  because stage 3 demonstrated that ±0.3% wall-clock movements on this path are
  binary layout.
- A module routine's `our @arr` store reaches its own container **without any
  consumer re-deriving the precedence** — i.e. `unit_lexical_slot` /
  `our_package_container_key` / `unit_lexical_container_cell` disappear from the
  store path rather than being reordered.
- `make test`, `make roast` and the battery gate stay green at every slice.

## 7. What this does *not* claim

- **It does not reach rakudo parity on the element store, and should not be
  started on that premise.** §1.4's accounting says roughly a third of the
  remaining 1,210 Ir is name-keyed probing and container re-lookup; the rest is
  `Value::view()` decoding and genuine itemization, which this ADR does not
  touch. §5's ≤150 ns bullet needs this *and* more.
- **It does not address the concurrent half of #8069** (§4.2-§4.4: the
  cross-thread lane, the read-side lock, the double refcount). Slice 5 is
  explicitly gated behind §4.2 for that reason.
- **It does not subsume ADR-0084.** That ADR moves entries that are not lexical
  variables *at all* out of the frame env; this one moves a lexical variable's
  own metadata onto its binding. They shrink the same map from opposite ends and
  neither blocks the other.

## 8. Alternatives rejected

- **Stop at `MetaNs` (i.e. declare #8087 done at stage 3).** Defensible on the
  perf axis — the keys are memoized and the gate prevents regrowth — but it
  leaves §1.5's correctness defect exactly as it is, and freezes the wrong design
  by making it cheap to keep. #8087's own issue text makes this argument against
  itself: "banning `format!` treats the symptom; the keys' existence is the
  disease."
- **Keep adding per-slot vectors as each campaign needs one.** This is the status
  quo and it demonstrably works, one property at a time, at the cost of twelve
  unrelated fields nobody reads as a whole and a thirteenth whenever the next
  profile lands. It also cannot fix §1.5, because each vector answers one
  question and the resolution-precedence bug is about a question nobody asked.
- **Put everything on the container** (ADR-0042 generalized). Right for
  container-valued properties and already done for the type constraint, but
  wrong for properties of the *name*: two names may share one container with
  different readonly-ness, and a sigilless alias is precisely a fact about the
  name rather than the thing it denotes.
- **A faster `Env`** (HAMT, or a per-namespace map). Treats the symptom, keeps
  the name→metadata indirection, and leaves §1.5 untouched.

## 9. Implementation status

Not started. Tracked by #8069 §4.1 and #8087 stage 4. Stages 1-3 of #8087
(`MetaNs` + the gate + all 48 namespaces funnelled through it) are merged and are
the prerequisite that makes slice 1 a change at one site per namespace rather
than at twenty-eight.
