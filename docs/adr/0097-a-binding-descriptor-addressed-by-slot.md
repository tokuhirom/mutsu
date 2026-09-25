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
   `shaped_array_dims`, `constant_var`, ~~`deep_readonly`~~. All were meant to
   be fixed at the declaration, and the first two already have an
   authoritative home on the container: ADR-0042 slice 3 deleted the
   `var_type_constraints` side table, but the `__mutsu_type::<name>` env key
   outlived it and `var_type_constraint_sym` still probes it. So this slice
   mostly *deletes a second source of truth* that ADR-0042 left behind rather
   than inventing a new home. §10 found this grouping is not actually uniform
   (`deep_readonly`'s hot half and `shaped_array_dims` are dynamic/per-call,
   not declaration-settled); §12 retired `deep_readonly` on its own, onto
   `ReadonlyKind` rather than `BindingDesc`.
3. **The binding-shape properties**: `sigilless_alias`, `sigilless_readonly`,
   `scalar_bind_no_container`, `bound`, `bound_decont`, `var_source_name`,
   `outer`. These are what the scalar store cascade probes on every assignment.
   §13 found this grouping is not uniform either: five of the seven need the
   still-unbuilt, closure-capture-blocked runtime half (§13.2); `bound` and
   `bound_decont` do not and are the next concrete slice (§13.3).
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

**Slice 1 is merged** (#8305, 2026-09-13): `src/binding_desc.rs` and
`CompiledCode::binding_descs` exist on `main`, folding `plain_locals`,
`simple_scalar_locals` and the five key-symbol vectors into one
`Vec<BindingDesc>`. No namespace retired, no behaviour change, as designed.

Stages 1-3 of #8087 (`MetaNs` + the gate + all 48 namespaces funnelled through
it) are merged and are the prerequisite that makes each later slice a change at
one site per namespace rather than at twenty-eight.

**Slices 2-5 are not started as written.** §10 records why slice 2 as written
is not a uniform next step. One of its five properties has since been retired
on its own: see §12 (`deep_readonly`, folded into `ReadonlyKind` rather than
onto `BindingDesc` or the still-unbuilt runtime-half array). §13 found slice 3
is not uniform either: `bound`/`bound_decont` are verified low-risk
compile-time folds and are the next actionable slice; the other five of its
seven properties join `shaped_array_dims` behind the runtime half, whose
closure-capture story (§11.5, generalized by §13.2) is now the thing to
design before any of them can move.

## 10. Slice 2 is five properties of different shapes, not one fold (2026-09-15)

Investigating slice 2 before writing any code (`type`, `hash_key_type`,
`shaped_array_dims`, `constant_var`, `deep_readonly`) found that none of the
five is a drop-in extension of slice 1's mechanism — each needs either far more
call-site surface, or a piece of the design §2 describes but slice 1
deliberately did not build (the **runtime half**, per frame, parallel to
`locals`). Recorded here so the next slice does not have to re-derive this by
reading the same code again.

- **`type` / `hash_key_type`.** 150 and 19 call sites respectively (`grep -c`
  on `var_type_constraint*` / `var_hash_key_constraint*` outside their own
  definitions), an order of magnitude past slice 1's twelve fields. Several
  read sites hold only a `name: &str`, never a slot (attribute fallback in
  `var_hash_key_constraint_sym`, `interpolate_regex_scalars`-style
  post-compile consumers) — moving the write side onto a slot-addressed
  descriptor still leaves those reads needing a name-keyed lane, so this is
  not a clean retirement, only a partial one. The scoping semantics
  (`set_var_type_constraint_routine_scoped` vs `_decl`, ADR-0042 slice 3's
  Text::CSV history) are exactly the kind of thing #8107 warns gets fixed by
  testing, not by reasoning — this namespace needs its own, careful slice.

- **`constant_var`.** Only 3 real sites, but
  `Interpreter::collect_eval_user_value_term_names` (`system_eval_string.rs`)
  answers "every constant name currently in scope" by scanning
  `self.env.keys()` for the `__mutsu_constant_var::` prefix, for EVAL bareword-
  term resolution. A slot-addressed descriptor cannot answer that question at
  all — there is no slot to enumerate *from* without first knowing the name,
  which is the thing being asked for. The write site's own comment explains
  the deeper reason this one may not be worth retiring even before that:
  `interpolate_regex_scalars` "runs long after compilation, on a bare `&self`
  with no compiler access", i.e. its reader never has a slot number to look
  one up with. Not every name-keyed env entry is this ADR's target; a
  consumer that has never had anything but a name is not a "name used as an
  address", it is a name used as a name.

- **`deep_readonly`.** The declaration-settled use
  (`methods_mut_method_lvalue.rs`) is one of two. The other, and the one that
  actually runs hot, is `vm_for_loop_body.rs`'s for-loop topic marker: `$_` is
  marked and unmarked deep-readonly on every loop entry/exit depending on the
  loop parameter's `is rw`-ness — dynamic, per-iteration, mutable state, not a
  fact fixed at a `my`. (It reaches env through a literal
  `"__mutsu_deep_readonly::_"`, not `MetaNs`, which is why
  `check-magic-keys.sh` — matching only an interpolating `format!` literal —
  does not flag it; a fixed literal for a fixed name has nothing to memoize.)
  This half belongs with the **runtime half** below, not slice 2's premise.

- **`shaped_array_dims`.** Confirmed against real `raku` that shape dims are
  genuinely per-invocation, not per-declaration-site:
  `sub f($n) { my @a[$n;$n]; ... }; f(2); f(4);` prints `(2 2)` then `(4 4)`.
  `CompiledCode.binding_descs` is one `Vec` per compiled chunk, shared by every
  call — exactly what slice 1's five properties are safe to live on, because
  they truly are fixed at compile time. Caching a per-call value there would
  be a correctness regression the first time a shaped declaration's dims
  depend on a parameter. This needs the runtime half of §2 (an array parallel
  to a *frame's* `locals`, not to the chunk's `CompiledCode`) — undesigned and
  unbuilt: no frame-lifetime-scoped, slot-indexed storage exists yet, and
  working out its lifecycle across closures and thread handoff is itself
  slice-sized work, not a fold.

**Conclusion:** slice 2's own grouping conflates "declaration-settled" with
"compile-time-fixed" — `shaped_array_dims` and half of `deep_readonly` are
neither. A useful next slice is narrower than the ADR's own list: either
build the runtime-half array first (which unblocks `shaped_array_dims` and
the loop-topic half of `deep_readonly` at once), or take `constant_var` on
its own once someone has decided its EVAL-enumeration consumer's fate, or
take `type`/`hash_key_type` alone as a dedicated slice given its call-site
count and history. Bundling all five, as written, is not one PR.

## 11. A second, distinct consumer: the `GetLocal`/Tier-B spoiler latch (#8748, 2026-09-19)

[#8748](https://github.com/tokuhirom/mutsu/issues/8748) reported a *different*
name→metadata indirection than the one slices 2-5 investigate, but the same
disease: the interpreter's `GetLocal` fast path (#8332) and the JIT's Tier B
inline local read (ADR-0004 J4d) both gate on
`crate::vm::vm_jit::LOCAL_READ_SPOILERS`, one process-global, monotonic,
never-decremented `AtomicU32`. Packing a single `ContainerRef` cell or `Proxy`
anywhere in the process — one `my $x := $y` on line 1 of an unrelated file —
bumps it forever, which disables the fast local read for **every slot, in
every frame, for the rest of the process**, including slots that are plain
`Int`s with no cell, no alias, and no relation to the bind at all. Measured
cost: +21.7% Ir with the JIT on, +11.6% with it off, on a loop that never
reads the spoiled variable (see the issue for the full repro and profile).

### 11.1 Why this is the same shape of problem as §1, and why it is not slice 2-5's problem

`LOCAL_READ_SPOILERS` already has a *static* half:
`CompiledCode::local_read_plain(idx)`, a per-slot, name-derived bit computed
once per chunk (attribute slots, `!`/`@`/`%`/`.`-prefixed names are
statically excluded). The interpreter and JIT both gate on
`local_read_unspoiled() && code.local_read_plain(idx)` — but because the left
operand is one global bit shared by every slot in every chunk, a real,
per-slot static classification is short-circuited into uselessness the
moment *any* cell exists *anywhere*. This is exactly §1.5's finding restated:
a process-wide flag is standing in for a question that is actually about one
binding's own history, not a fact about the process.

It is a **separate** consumer from slices 2-5, not a sixth item on that list:
`BindingDesc` answers questions about a binding's own declared shape
(sigilless, bound, typed, ...), while this latch answers a question about
*any* binding's dynamic history (was a cell ever created here). It shares
slice 1's design principle — collapse a process/name-keyed probe into a
per-slot, compile-time-resolved one — but touches different code (the JIT's
emitted guard, not the scalar-store cascade) and has its own, independent
correctness surface. It gets its own section rather than a slice number
because, unlike slices 2-5, part of it is investigated below to the point of
a concrete, testable mechanism (already landed as a data-only slice), which
none of slices 2-5 reached.

### 11.2 `LOCAL_READ_SPOILERS` is actually four unrelated sources, of two different shapes

Reading `note_local_read_spoiler`'s callers (`src/vm/vm_jit.rs:129-159`)
splits the latch into two classes:

- **Slot-addressable in principle**: a `ContainerRef` cell
  (`CONTAINER_CELLS`) or a `Proxy` (`note_proxy_value`) packed into *this
  slot*. The compiler can, in principle, know which local names are ever the
  target of such a value — this is a static fact about the chunk's own
  bytecode, the same kind of fact `local_read_plain` already is.
- **Inherently name/dynamic-scope-based, not slot-addressable from this
  chunk's compilation**: a `$CALLER::x := ...` alias (`CALLER_VAR_BINDS`)
  targets a variable in some *caller* frame by name and depth, resolved at
  call time — the compiling chunk cannot know which of its own slots a
  distant caller's `$CALLER::` will name. The two remaining sources
  (`Interpreter::atomic_var_seen`, `Interpreter::sigilless_attrs_active`) are
  *per-interpreter* runtime flags, not compile-time facts about any chunk at
  all.

So a sound per-slot answer can only ever cover the first class. The second
class has to stay a dynamic (if no longer necessarily process-global) latch
regardless of how precise the first class becomes.

### 11.3 Investigating the first class found the existing analysis is incomplete, not just process-global

The natural place to look for "does this chunk already know which slots
receive a `:=`/Proxy value" is `CompiledCode::scalar_bind_locals` (§1.3's
table: "the slots declared `my $x := …`") and `OpCode::TagContainerRef`,
which every `:=`-shaped compile site already emits with a
`source_slot: Option<u32>` resolved from `self.local_map` — i.e. the
compiler already computes the exact target slot at every one of these sites,
it just does not record it anywhere durable. Auditing every
`TagContainerRef`/`TagContainerRefReversed` emission site
(`compiler/control_for.rs`, `compiler/expr_block.rs`, `compiler/expr_data.rs`,
`compiler/helpers_block_inline.rs`, `compiler/stmt.rs`) plus
`scalar_bind_locals`'s own call site found the coverage is **not** what a
first read suggests:

- `scalar_bind_locals` only fires for a **scalar** `my $x := …`
  declaration (`is_scalar_colon_bind` explicitly excludes `@`/`%`) — the
  issue's own repro, `my @unused := @data;`, is an **array** bind and is
  invisible to it.
- A **statement-level rebind with no `my`** (`$x := $y;`, targeting an
  already-declared lexical) emits *no* `TagContainerRef` at all — it
  compiles straight to `SetLocal`/`SetGlobal` (`stmt.rs`'s `AssignOp::Bind`
  arm, `emit_set_named_var`), so this shape was previously invisible to both
  `scalar_bind_locals` and every `TagContainerRef` site.
- The remaining `TagContainerRef` sites (expression-context `:=`, `for`-loop
  `is rw` source aliasing, `given`/`when`/tail-position topic
  container-writeback) do resolve a slot via `self.local_map`, but nothing
  collected it.

This matters beyond completeness for its own sake: it is a second instance of
§1.5's warning that "a name is not an address" — the *existing* per-name
tracking (`scalar_bind_locals`) was itself missing two of the four shapes a
`:=` can take, and would have been silently wrong as the sole basis for a
fast-path relaxation.

### 11.4 What has landed: a data-collection-only slice, no behaviour change

Following exactly the precedent of slice 1 ("no behaviour change, no
namespace retired — this is the consolidation that makes every later slice a
one-site change"), this PR adds:

- `CompiledCode::rebind_target_slots: Vec<u32>` — every slot ever recorded as
  a compile-time-resolved `TagContainerRef`/`TagContainerRefReversed` target,
  fed by a new `CompiledCode::note_rebind_target(Option<u32>)` called from
  all eight existing emission sites, from the declaration-time array/hash
  bind path (previously only `scalar_bind_locals`, and only for scalars),
  and from the statement-level no-`my` rebind path identified in §11.3 (which
  had no `TagContainerRef` to piggyback on before this PR).
- Five Rust unit tests (`opcode::local_may_be_celled_tests`) compiling real
  source and asserting the exact repro shape from #8748: an unrelated
  `my @unused := …` marks only its own slot, a plain program with no `:=`
  anywhere marks nothing, and the previously-uncovered no-`my` rebind and
  expression-context rebind shapes are both tracked.

**The field is written but not read by any execution path yet.** A first
version of this slice also added a memoized `local_may_be_celled(idx) ->
bool` accessor (mirroring `local_read_plain`'s `OnceLock<Box<[bool]>>`
pattern) with a conservative `true` default — but with no real caller, that
method and its backing field were genuinely dead code, and
`scripts/check-panic-surface.py`'s `#[allow(` ratchet (#8186) correctly
rejected the two `#[allow(dead_code)]` markers it would have taken to ship
them. Rather than force the ratchet open for a preparatory accessor, the
per-slot memo was pushed down into the test module itself (a small
`may_be_celled(code, idx)` test helper reading `rebind_target_slots`
directly) — production code carries only the raw, write-only `Vec<u32>`,
which needs no allow because it is genuinely exercised by real compile
sites. The follow-up wiring slice is what gives the memoized accessor a real
caller and earns it back into production code.

This incompleteness is intentional: §11.5 lists what is still open before
wiring a per-slot answer into `local_read_unspoiled`'s gate would be sound,
and getting that wrong is not a `make roast` red — it is a fast path
silently serving a stale value for a program that happens not to be in the
roast/`t/` corpus. ADR-0097 §1.5 records two prior instances of exactly this
failure mode, both caught only by a pre-existing test, not by review.

### 11.5 What is still open before wiring this in

- **Closure capture is not covered at all.** A nested `sub`/closure that
  captures an outer lexical and later binds or mutates it through a shared
  cell does not go through `self.local_map` in the *outer* chunk's own
  compilation (a true closure boundary gets its own `CompiledCode` and
  reaches outer names through the upvalue mechanism, not `SetLocal`), so
  `rebind_target_slots` cannot see it from the outer chunk alone. Whether the
  compiler's existing closure-capture bookkeeping (built to construct the
  upvalue array) already identifies, from the *outer* chunk's perspective,
  which of its own slots are captured by an inner closure needs its own
  investigation before this can be folded in as a third source alongside
  `TagContainerRef` and the declaration path.
- **The site audit in §11.3 is not proven exhaustive.** It covers every
  `TagContainerRef`-shaped bind this investigation found, plus the one
  no-`my`-rebind gap, but (per closures above) is known to be incomplete,
  and a codebase this size may have other bind-shaped constructs (parameter
  binding forms, sigilless `\`-capture, sub-signature destructuring) that
  were not audited here.
- **Wiring it in must not simply replace the global check** — the residual
  sources (§11.2's second class: `CALLER_VAR_BINDS`, the two per-interpreter
  flags) still need a latch, whether that stays the current
  `LOCAL_READ_SPOILERS` word (with `CONTAINER_CELLS`/`note_proxy_value` no
  longer contributing to it once the per-slot answer subsumes their case) or
  something narrower.
- **The JIT side is comparatively low-risk once the interpreter side is
  proven**: because the classification is per-chunk and compile-time-fixed
  (exactly like `local_read_plain`), `vm_jit_tier_b.rs`'s `emit_get_local`
  would bake the per-slot answer into the emitted-code eligibility decision
  the same way it already does for `local_read_plain` — no new invalidation
  story, since a `false` answer never becomes `true` later for the same
  chunk.
- **Verification approach for the wiring slice**: the existing
  `debug_assert!` block in `exec_get_local_op_inner`
  (`vm_var_assign_local_get.rs:189-202`) already cross-checks the *global*
  latch's three readable sources against reality on every fast-path hit in
  debug builds, exercised by the `gc-stress-tap`/`jit-stress-tap` CI jobs.
  The wiring slice should extend that same assertion to check
  `local_may_be_celled(idx)` against `self.locals[idx]`'s actual runtime kind
  before trusting it to gate anything in release, giving the closure-capture
  gap (and any other unaudited source) a real chance to surface as a debug
  assertion rather than a silent wrong answer.

## 12. `deep_readonly` retired on its own (2026-09-19)

§10 named `deep_readonly` as one of slice 2's five properties and immediately
disqualified it from that grouping: its hot consumer
(`vm_for_loop_body.rs`'s `for`-loop topic marker on `$_`) is dynamic,
per-iteration state, not a fact fixed at a `my` — so it needs "the runtime
half", the frame-lifetime array parallel to `locals` that §10's own
conclusion listed as the first of three candidate next steps, alongside
taking `type`/`hash_key_type` alone or settling `constant_var`'s fate.

Reading the actual call sites before starting that array's design found a
narrower resolution: `Interpreter::readonly_vars` (`src/runtime/mod.rs`) is
*already* the general "dynamic, per-frame, `Symbol`-keyed, scope-journaled"
mechanism the runtime half is reaching for — a `RefCell<ReadonlySet>` with
proper mark/unmark/restore and undo-on-scope-exit, used for exactly this
binding's OTHER readonly fact (`ReadonlyKind::Immutable`, marked on the very
same `"_"` at the very same call sites). The env marker duplicated a
mechanism that already existed one field away, rather than needing a new one
built from scratch.

`ReadonlyKind` gained a fourth variant, `ImmutableDeep`: `Immutable`'s
"Cannot assign to an immutable value" refusal, plus the method-mutation
refusal (`.value = ...`) the env marker used to carry on its own. This
retires `MetaNs::DeepReadonly` and the `__mutsu_deep_readonly::` namespace,
and as a side effect fixes a bug the split representation caused:
`restore_topic_readonly` could only restore the `ReadonlyKind` half on loop
exit, so a `for`-loop over an immutable `QuantHash` whose body ran a nested
`for`-loop lost its own deep-readonly mark the moment the inner loop
restored `"_"`. Folding the two facts into one mark makes that
unrepresentable, the same argument §3.2 makes for the slot-addressed
descriptor proper — a fact split across two independently-updated stores is
a bug waiting for the update that forgets one of them.

This is deliberately **not** the runtime-half array §10 sketched, and does
not build it. `shaped_array_dims` still needs that array (or some other
per-invocation, slot-indexed home) — verified against real `raku` to be
genuinely per-call, so it cannot move onto `CompiledCode.binding_descs`
either. What this section adds is one data point: not every property §10
called "needs the runtime half" turns out to need a *new* one — check
whether an existing per-frame dynamic mechanism already answers the same
shape of question before designing another.

## 13. Slice 3 is two shapes again, and one of them needs no runtime half at all (2026-09-21)

Investigating slice 3 (`sigilless_alias`, `sigilless_readonly`,
`scalar_bind_no_container`, `bound`, `bound_decont`, `var_source_name`,
`outer`) before writing code, per the §10/§12 precedent.

### 13.1 A counting mistake worth recording

A first pass counted real call sites with `grep -c "MetaNs::Bound"`, which is
a **substring** match: it silently folded in every sibling namespace whose
name starts with `Bound` (`BoundIndex`, `BoundArrayLen`, `BoundArraySlice`,
`BoundDecont`), inflating `Bound`'s apparent count to ~20. Re-run with a
word-boundary match, `Bound` has **5** real call sites and `BoundDecont` has
**5** — both far smaller than slice 2's `type`/`hash_key_type`, and in the
same range as slice 1's twelve fields. Anyone re-deriving call-site counts
for a `MetaNs` variant whose name is a prefix of others' should use
`grep -rnE "MetaNs::Name\b[^A-Za-z]"` or equivalent, not a plain substring
match.

### 13.2 Five of the seven are the same dynamic shape as `shaped_array_dims`

`sigilless_alias`, `sigilless_readonly`, `scalar_bind_no_container`,
`var_source_name`, and `outer` all fail the same test §10 applied to
`shaped_array_dims`: the *value* held is decided at **runtime**, not at the
declaration's compile time, and can differ across repeated executions of the
same slot (different loop iterations, different calls, different bind
targets):

- `sigilless_alias` stores the **resolved alias-chain target name** (`my \x
  := \y`), walked with a `while seen.insert(...)` loop at the *store* site
  (`vm_misc_assign.rs`) — a graph, not a bit, and the target can be a
  parameter or a computed value unknown until the bind executes.
- `scalar_bind_no_container`'s own write-site comment states the shape
  directly: "Set/cleared per declaration so a **later** `my $o = 5` of the
  same name goes back to owning a Scalar" (`vm_var_assign_set_local.rs:1186`).
  That "later" is a different call or loop iteration re-executing the same
  bytecode, not a different compile-time site.
- `var_source_name` is written once **per call**, inside the parameter
  binder (`binding_signature.rs`), recording which caller-side variable this
  invocation's `@`/`%` parameter aliases — necessarily different across
  calls.
- `outer` is a **per-closure-creation value snapshot** (`vm_register_ops.rs`),
  keyed by name inside the flattened capture `Env` a closure gets at
  creation time, not a fact about a compiled slot at all — a program can
  create many closures over the same lexical, each with its own snapshot.

All five need "the runtime half" — a frame-lifetime, slot-indexed store,
undesigned per §10 — or, per §12's precedent, an existing dynamic
per-frame mechanism that already answers the same shape of question. None of
the five fits `readonly_vars`/`ReadonlyKind` the way `deep_readonly` did (they
are not readonly facts), so unlike §12 this is not a one-line reuse. Building
the runtime half generically was investigated as part of this round and
rejected for now: `Locals` (`src/runtime/locals.rs`, ADR-0077) is the right
place to add a parallel slot-indexed array with no call-site changes (its
`push_frame`/`pop_frame`/`refill_slots`/`resize_slots` are the only places
that would need to grow it in lockstep), but every one of these five
properties is *also* readable through a closure's captured `env` today —
`outer`'s own mechanism is explicitly built around exactly that — and
`Locals` does not travel with a closure capture. A `Locals`-only runtime half
would silently lose the fact the moment any of these five crosses a closure
boundary, which is the same gap §11.5 already named as unresolved for the
`LOCAL_READ_SPOILERS` wiring slice. That section's "closure capture is not
covered at all" is not specific to that one latch — it is the general
blocker for every property in slices 2-5 that a closure can reach, and it
should be answered once, for the runtime half as a whole, before any of
these five is attempted.

### 13.3 `bound` and `bound_decont` are a different, better shape

Unlike the five above, `bound`'s and `bound_decont`'s *write* sites are each
one fixed statement — `Stmt::MarkBoundContainer` (`compiler/stmt.rs:1131`)
and the coerce-path bind marker (`vm_var_assign_coerce.rs`) — compiled once
per `:=`-to-container declaration. `Compiler::alloc_fresh_local`
(`compiler/mod.rs:2002`) never reuses a slot number across declarations: it
always pushes `self.code.locals.len()` as the new slot, so **exactly one
declaration statement ever owns a given local slot** for the life of a
compiled chunk. That means "is slot N a `:=`-bound container" is not a
per-execution fact that can drift between calls or iterations the way
`scalar_bind_no_container`'s is — it is fully decided by which AST statement
compiled to that slot, i.e. at compile time, for any name that resolves to a
local slot at all. The existing "clear a stale marker" defenses elsewhere in
this file (`bound_array_slice`, `bound_decont` itself) exist because *env* is
name-keyed and two different slots can share a name across sibling scopes,
producing spurious staleness on the shared string key — a bug class that is
specific to name-keying and cannot occur once the fact is addressed by the
declaration's own permanent slot.

So `bound` (read hot: `OpCode::CheckReadOnly` "runs on every whole-variable
assignment... per iteration in tight loops" per its own comment,
`vm_exec_dispatch.rs:6023`) is a genuine slice-1-shaped candidate — a
compile-time bit on `BindingDesc`, needing **no runtime write and no runtime
read at all** for the local-slot case, which is the common one. What is not
yet verified: `Stmt::MarkBoundContainer` compiles to `OpCode::SetGlobal`, not
a local-slot op, so it may also fire for a name with no local slot (`our
%a := ...` at file/package scope) — that case would still need the current
env path, the same "partial retirement" shape already accepted for
`type`/`hash_key_type` in §10. This needs verifying against the compiler's
`our`/package-scope lowering before implementation, and the surrounding
`CheckReadOnly`/`is_ro_constant_hash` logic (distinguishing a bound Map from
a `constant` one) is intricate enough that the fold should land with its own
focused regression tests before touching the hot path.

### 13.4 Conclusion and next slice

Slice 3, like slice 2, is not one fold. Five of its seven properties join
`shaped_array_dims` behind the still-unbuilt, closure-capture-blocked runtime
half — and closure capture is the thing to design next if that half is to be
built at all, not any one property's plumbing. `bound` and `bound_decont`
are the exception: they are slice-1-shaped, verified low-risk by the
`alloc_fresh_local`-never-reuses-slots invariant, and are the next concrete,
scoped implementation slice — smaller than originally estimated (5 call
sites each, not the ~20 a substring-match miscount suggested) once the
local-slot-vs-package-scope split above is confirmed.

## 14. A closure's view of a rebind: the binding cell (#9237, 2026-09-24)

[#9237](https://github.com/tokuhirom/mutsu/issues/9237) is the §11 caveat
("closures that capture and rebind an outer lexical are a second, distinct
source of celling") showing up as a wrong answer. Rakudo's closure reads the
lexical *pad slot*, so it sees `$a := X` made after it was created. mutsu's
closure captured the variable's *container cell*. A rebind can only replace the
frame slot, and writing through the shared cell would re-bind a second name
bound earlier with `my $f := $a` (#9207).

The fix adds one level of indirection, only where it is needed:

- **Compile time.** `CompiledCode::rebound_slots` records the slots that a
  statement- or expression-level `:=` rebinds after their declaration. It is
  narrower than `rebind_target_slots`, which also holds declaration binds and
  plain-assignment `TagContainerRef` targets.
- **Capture.** When `box_captured_lexicals` boxes a captured scalar whose slot
  is in `rebound_slots`, it gives the slot a **binding cell** `B` whose content
  is the container `C` (`Interpreter::binding_cell_of` / `wrap_in_binding_cell`).
  The frame slot, its env entry and every capturing closure share `B`.
- **Reads and writes** need no new code. `Value::with_deref` / `into_deref`
  already collapse a cell-holding-cell chain, and `Value::store_through_cell`
  already writes a plain value through to the innermost cell. #8759 left the
  same shape behind for rw-parameter rebinds.
- **Rebind.** `exec_set_local_op` notes `B` before the store. After the store it
  moves the new binding into `B` and puts `B` back in the slot
  (`reseat_binding_cell`).
- **Binding another name to it.** `my $g := $a` binds `$g` to the innermost
  container `C`, not to `B`. The source's own slot keeps `B`, so a later rebind
  of `$a` leaves `$g` alone.

What this does not cover: captures that bypass `box_captured_lexicals` (the
non-escaping, value-frozen `owned_captures`). That shape was not reported. If
one turns up, the same binding cell is the place to extend.

### 14.1 A rebind made inside a closure (#9307, 2026-09-25)

`{ $a := $b }` rebinds a *free* variable, so it has no own slot to record in
`rebound_slots`, and the sibling closures kept the old container.

- **Compile time.** A `:=` with no own slot records its target name in
  `CompiledCode::rebound_free_names`. `compute_free_vars` folds those names and
  every nested closure's `free_var_rebinds` upward. The chunk that declares the
  name adds all of the name's slots to `rebound_slots`, and the other names
  continue up as its own `free_var_rebinds`. The declaring frame then boxes the
  capture into a binding cell exactly as in §14.
- **Rebind.** The closure's rebind is a by-name `SetGlobal`. It notes the
  binding cell in its env entry before the store. After the store it moves the
  new binding into the cell and puts the cell back in the env entry
  (`reseat_env_binding_cell`). Both store paths do this: the variable-to-variable
  bind that returns early and the general path. The frame and every sibling
  closure share the cell, so they see the new binding, and the call-return
  writeback carries the same cell back to the frame's slot.
