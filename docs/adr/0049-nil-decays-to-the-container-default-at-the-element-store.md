# ADR-0049: `Nil` decays to the *container's* default at the element store, and stops being a hole sentinel

- **Status**: Accepted (Slices 0-6 implemented; see "Implementation status" below)
- **Date**: 2026-08-20
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md) (the same Raku model —
  "an Array/Hash element *is* a `Scalar` container" — taken on the *itemization* surface; this ADR takes
  the *Nil-decay* surface, and both land at the same store sites),
  [ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md) §7 (the *aliasing* surface),
  [ADR-0042](0042-type-constraints-belong-to-the-container-not-to-a-name.md) (the `var_type_constraint(name)`
  side table today's narrow fixup is gated on — see §5.3),
  `todo/deep/array-literal-nil-not-decayed-at-construction.md` (the originating finding, which this ADR
  supersedes and which slice 6 retires)

> A Raku `Array`/`Hash` element is a `Scalar` container, and a `Scalar` cannot hold `Nil`: assigning
> `Nil` to it restores the container's *default*. mutsu implements that rule at roughly twenty
> assignment sites, misses it at every construction site, and then compensates for the miss in two
> renderers and at a dozen read sites. Worse, the value that should be impossible in a real element —
> `Nil` — is simultaneously mutsu's sentinel for "absent key", for "deleted slot", and for
> "autovivification gap", with `Package("Any")` serving as the *intended* gap marker everywhere else.
> This ADR decides to apply the decay at the element *store*, targeting the owning container's default,
> and to make `ArrayData::initialized` the sole hole discriminator.

---

## 1. Context

### 1.1 What Raku specifies

Two rules, and their interaction is the whole subject of this ADR:

1. **An `Array`/`Hash` element is a `Scalar` container.** (This is ADR-0040's premise, restated.)
2. **Assigning `Nil` to a container restores that container's default** — `Any` for an untyped one, the
   element *type object* for a typed one, a native zero for a native one, and the `is default(...)`
   value when one is declared. `raku -e 'my $x = 5; $x = Nil; say $x.WHAT'` → `(Any)`;
   `my Int @t; @t[0] = Nil` → `(Int)`; `my @c is default(42); @c[1] = Nil` → `42`.

Together they mean: **no element of a real `Array`/`Hash` can ever hold `Nil`.** Not after an
assignment, not after a `push`, and not after literal construction — `[Nil]` is `[Any]` the instant it
is built, before anything is done with it.

The rules stop at the container boundary, and that boundary is exactly the discriminator mutsu needs. A
`List`'s elements are *not* containers, so `Nil` survives there: `(1, Nil, 2)[1]` is `Nil` in raku, and
a slurpy `*@x` (which is `List`-backed) keeps `Nil` too. `Nil` is also raku's own projection of an array
*hole* when one is materialized as a list — `my @a; @a[2] = 5; @a.List.raku` is `(Nil, Nil, 5)`. So the
decay must be scoped to real mutable containers, and `Nil`-in-a-`List` must stay untouched.

### 1.2 mutsu implements rule 2 at the assignment sites and nowhere else

The decay is present, and in places quite carefully done. `vm_var_assign_index_named.rs:691-703` is the
main element-assign decay and implements the full ladder correctly (`is default` → constraint's nominal
type object → `Any`, with `:=` and `is default(Nil)` correctly exempted). `vm_data_push_ops.rs:9-42`
(`push_nil_to_elem_default`) does the type-aware version for `push`, including per-element `Slip`
expansion. `vm_var_assign_typed.rs:322-443` covers typed hash values, native arrays, shaped
re-seeding, and typed array elements. `methods_mut_dispatch.rs:718-730` covers the slow-path listops,
and `methods_mut_method_lvalue.rs` covers `is default(...)` attributes in four places. That is ~20
independent implementations of one rule, no two of them shared.

The one that carries the *list-assign* case is the narrow fixup in `exec_set_local_op_inner`
(`src/vm/vm_var_assign_set_local.rs:974-991`), with a byte-identical sibling for the `AssignExpr` form
at `src/vm/vm_var_assign_local.rs:156-170`:

```rust
// An untyped `@` assignment resets Nil elements to Any (their
// fresh containers' default; `my @a = (1,2)[1,2]` is `[2, Any]`).
if !is_bind
    && loan_env!(self, var_type_constraint(name)).is_none()
    && let ValueView::Array(items, kind) = assigned.view()
    && kind.is_real_array()
    && items.iter().any(Value::is_nil)
{ … *data.items_mut() = crate::runtime::utils::nil_elems_to_any(old_items); … }
```

Every gate is a place the rule leaks. `!is_bind` excludes binding. `var_type_constraint(name).is_none()`
excludes typed arrays. Keying on an `@`-sigiled *variable name* excludes anonymous construction
entirely. And `nil_elems_to_any` (`src/runtime/utils/coerce_containers.rs:355-369`) hardcodes `Any`, so
it could not serve a typed or `is default(...)` container even if it were reached — which is why all
nine of its call sites must gate themselves to untyped arrays first.

**The construction paths perform no decay at all**, and two of them actively do the wrong thing:

| site | Nil handling |
| --- | --- |
| `exec_make_array_op` (`src/vm/vm_data_ops.rs:4-149`) | none — `Nil` falls to `_ => elems.push(val)` at `:135` |
| `exec_make_array_no_flatten_op` (`:152-166`) | **drops the element**: `ValueView::Nil => {}` at `:160` |
| `exec_make_hash_op` / `exec_make_hash_from_pairs_op` (`:168`/`:190`) | none |
| `build_hash_from_items_with_key_coercion` (`src/runtime/utils/coerce_containers.rs:256-345`) | none — `boxed_val.clone()` verbatim at `:281`/`:316`/`:341` |
| `coerce_to_hash` (`:14-160`) | none, and **inserts `Value::NIL`** for a trailing odd key (`:314`, `:349`) |
| `coerce_to_array` (`:371-409`) | none, deliberately — its comment says "this coercion is type-blind, and the assignment sites convert them" |
| `try_native_array_construct` / `try_native_hash_construct` (`src/runtime/methods_aggregate_ctor.rs`) | none; typed `Array[Int].new(Nil)` type-checks the raw `Nil` with no `is_nil` exemption, unlike `vm_var_assign_typed.rs:425` |
| parameterized `Array[T].new` / `Hash[V,K].new` (`src/runtime/methods_object_dispatch_new.rs:1289`/`:1384`) | none — separate re-implementations |

`coerce_to_array`'s comment is the finding in miniature: it is *correct* about the design intent and
*wrong* about the world, because there is no assignment site between a `[Nil]` literal and, say, a sub
call that binds it.

### 1.3 The divergence, measured on `main` (227e38e4f, 2026-08-20)

Each row is its own block so nothing leaks between them (`tmp/nilfinal.raku`, `tmp/nil4.raku`,
`tmp/nil6.raku`, `tmp/nilverify.raku`). Every divergence traces to one construction of a real container
that skipped the decay.

| # | program | raku | mutsu |
| --- | --- | --- | --- |
| 01 | `my @b = [Nil]; @b eqv [Nil]` | `True` | `False` |
| 02 | `[Nil] eqv [Any]` | `True` | `False` |
| 03 | `[Nil][0].WHAT` | `(Any)` | `Nil` |
| 04 | `my @a := [Nil]; @a[0].WHAT` | `(Any)` | `Nil` |
| 05 | `sub f(@x){@x[0].WHAT}; f([Nil])` | `(Any)` | `Nil` |
| 06 | `my $c = [Nil]; $c[0].WHAT` | `(Any)` | `Nil` |
| 07 | `[Nil,1][0]:exists` | `True` | **`False`** |
| 08 | `[Nil,1][0]:v` | `(Any)` | `()` (empty) |
| 09 | `[Nil,1].head.WHAT` | `(Any)` | `Nil` |
| 10 | `[Nil,1].map({.WHAT}).head` | `(Any)` | `Nil` |
| 11 | `[Nil,1].sort.raku` | `(Any, 1).Seq` | `(Nil, 1).Seq` |
| 12 | `[Nil,1].reverse.raku` | `(1, Any).Seq` | `(1, Nil).Seq` |
| 13 | `[Nil,1].flat.raku` | `(Any, 1).Seq` | `(Nil, 1).Seq` |
| 14 | `[Nil,1].clone[0].WHAT` | `(Any)` | `Nil` |
| 15 | `[[Nil]] eqv [[Any]]` | `True` | `False` |
| 16 | `Array.new(Nil)[0].WHAT` | `(Any)` | `Nil` |
| 17 | `Array.new(Nil)[0]:exists` | `True` | **`False`** |
| 18 | `[Nil].List.raku` | `(Any,)` | `(Nil,)` |
| 19 | **`[Nil,].elems`** | `1` | **`0` — the element is silently dropped** |
| 20 | `{a=>Nil} eqv {a=>Any}` | `True` | `False` |
| 21 | `my %g = a=>Nil; %g eqv {a=>Any}` | `True` | `False` |
| 22 | `{a=>Nil}.values.head.WHAT` | `(Any)` | `Nil` |
| 23 | `{a=>Nil}.pairs.head.value.WHAT` | `(Any)` | `Nil` |
| 24 | `{a=>[Nil]} eqv {a=>[Any]}` | `True` | `False` |
| 25 | `my %h; %h.AT-KEY("missing").WHAT` | `(Any)` | `Nil` |
| 26 | `my %n = a=>Nil; %n.AT-KEY("a").WHAT` | `(Any)` | `Nil` |
| 27 | `my Int @a = [Nil]` | **dies**: `Type check failed for an element of @a; expected Int but got Any (Any)` | `Array[Int].new(Int)` — succeeds |
| 28 | `my %h{Int} = 1 => Nil` | `(my Any %{Int} = 1 => Any)` | **dies**: `Type check failed in assignment to ; expected Int but got Nil ()` |
| 29 | `my @d is default(42) = [Nil]; @d[0]` | `(Any)` | `42` |

Four rows deserve to be read closely, because each closes a design option on its own:

- **Row 19 is data loss.** `exec_make_array_no_flatten_op`'s `ValueView::Nil => {}` drop means a
  one-element array literal silently becomes empty. No read-side or render-side fix can recover an
  element that was never stored.
- **Row 18 is the sentinel collision made visible in one line.** `.List` is *correct* to materialize a
  hole as `Nil` (that is raku's rule, pinned by `t/nil-list-holes.t` and reproduced faithfully by mutsu
  — see I4 below). It is *wrong* here, on the same operation, because mutsu cannot tell a real `Nil`
  element from a hole. Same code, right answer for a hole, wrong answer for a value.
- **Rows 27/28** are mutsu being lenient where raku dies *and* dying where raku is fine, in the same
  feature, both because an undecayed `Nil` reached a type check.
- **Row 25** shows the divergence is not even limited to stored `Nil`s: `.AT-KEY` on a *missing* key
  returns the raw `Value::NIL` sentinel because the read-side compensator (§1.6) is not on that path.

### 1.4 The invariants that already agree — and what they prove

The same run measured thirteen rows that mutsu already gets right. They are not decoration: they are the
acceptance boundary, and collectively they prove the fix is scoped to real-container construction and
nothing else.

| # | program | raku = mutsu |
| --- | --- | --- |
| I1 | `(1,Nil,2)[1].WHAT` — a `List` element stays `Nil` | `Nil` |
| I2 | `(1,Nil,2) eqv (1,Nil,2)` | `True` |
| I3 | `sub s(*@x){@x[0].WHAT}; s(Nil,1)` — slurpy is `List`-backed | `Nil` |
| I4 | `my @a; @a[2]=5; @a.List.raku` — holes materialize as `Nil` | `(Nil, Nil, 5)` |
| I5 | `my @a; @a[2]=5; @a[0].WHAT` | `(Any)` |
| I6 | `my @a; @a[2]=5; @a[0]:exists` | `False` |
| I7 | `my @n = 1,Nil,3; @n[1]:exists` | `True` |
| I8 | `my @n = 1,Nil,3; @n[1]:v` | `(Any)` |
| I9 | `my @d=1,2,3; @d[1]:delete; @d[1]:exists` | `False` |
| I10 | `my @z=1,2,3; @z[1]=Nil; @z eqv [1,Any,3]` | `True` |
| I11 | `my Int @t=1,2; @t[0]=Nil; @t.raku` | `Array[Int].new(Int, 2)` |
| I12 | `my Int @a=1,Nil,3; @a.raku` | `Array[Int].new(1, Int, 3)` |
| I13 | `[Nil].elems`, `[1,Nil].elems`, `[Nil].gist`, `[Nil].Slip.raku` | `1`, `2`, `[(Any)]`, `slip(Any,)` |

**I7/I8 versus rows 07/08 is the sharpest statement of the defect**: `my @n = 1, Nil, 3` and `[Nil, 1]`
are the same Raku construct, and mutsu answers `:exists` differently for them — `True` for the first,
`False` for the second — purely because the first goes through the `@`-variable fixup and the second
does not.

**I4 versus row 18 is the constraint that stops the naive fix.** `Nil` is still the correct
materialization of an array *hole* through `.List`, so "purge `Nil` from arrays" is wrong. The decision
has to be about the *store*, and holes have to be represented by something other than a stored `Nil`.

### 1.5 The discriminator experiment — the decay target is the *owning* container

Row 29 is worth isolating, because it decides *which* default is applied and rules out the shape of the
existing fixup:

```
my @d is default(42) = [Nil];  @d[0]                 # raku: (Any)   mutsu: 42
my @c is default(42) = 1,2,3;  @c[1] = Nil; @c[1]    # raku: 42      mutsu: 42   (agrees)
```

raku says `(Any)` for the first because the `[Nil]` literal is *itself* a real `Array` with no default,
so its element decays to `Any` at **its own** construction; the outer list-assign then merely copies an
already-decayed `Any` in. mutsu says `42` because its fixup runs at the *outer* variable and applies the
outer container's default to a `Nil` that should never have reached it.

So the rule is not "rewrite `Nil`s found in an array being assigned to a variable". It is
**"when a value is stored into an element of container C, a `Nil` becomes C's own default"**, applied at
each container as it is built. Getting row 29 right is free under that rule and impossible under the
current one.

### 1.6 The sentinel collision — `Nil` means four different things

This is what makes the finding architectural rather than a missing call to `nil_elems_to_any`.

**The intended gap marker is `Package("Any")`, not `Nil`.** When an untyped array is extended by
`@a[2] = 5`, `autoviv_resize` (`src/vm/vm_var_assign_ops.rs:59-73`) fills the skipped slots with
`native_fill_for_constraint(None)` = `Value::package("Any")` (`:11-35`; `int`→`0`, `num`→`0e0`,
`str`→`""`, boxed `T`→`Package(T)`), and `real_array_initialized_at`
(`src/value/value_methods_a.rs:266-272`) records which index was actually written. A shaped array is
seeded the same way and marked with `shaped_array_unassigned` (`:282-287`). **`ArrayData::initialized`
(`src/value/mod.rs:1020`) is therefore already a complete and precise hole discriminator**: `None` means
"bulk-constructed, every in-range index exists"; `Some(set)` means "element-wise assignment happened,
and only these indices exist". It is maintained by `mark_initialized_index`
(`src/vm/vm_var_index_tracking.rs:171-211`, called from the four element-assign opcodes in
`vm_var_assign_index_named.rs`), `unmark_initialized_indices` (`:310-345`, which materializes `(0..len)`
before removing, so a `.clone` inherits the hole), and `DELETE-POS`
(`src/runtime/methods_subscript_protocol.rs:89-97`). It is type-blind — untyped arrays get it too.

**And yet `Nil` is retained as a second, imprecise hole sentinel.** `ArrayData::hole_at`
(`src/value/value_collections.rs:268-279`) is otherwise a clean chokepoint — twelve call sites route
through it (`src/vm/vm_var_exists_ops.rs:762`, `src/runtime/builtins_multidim_subscript.rs:841`,
`src/runtime/builtins_multidim.rs:198`, `src/runtime/methods_subscript_protocol.rs:92`,
`src/runtime/methods_call_dispatch.rs:2657`, `src/builtins/methods_narg/dispatch_1arg.rs:598`,
`src/builtins/methods_0arg/coercion.rs:253`/`:347`,
`src/builtins/methods_0arg/dispatch_core_range.rs:88`, `src/vm/vm_var_multidim_helpers.rs:213`) — but
its body reads:

```rust
match self.items.get(i).map(Value::view) {
    None => true,
    Some(ValueView::Nil) => true,                       // <- the collision
    Some(ValueView::Package(name)) => {
        let is_gap_marker = name == "Any" || self.value_type…is_some_and(|t| name == t);
        is_gap_marker && self.initialized.as_ref().is_some_and(|s| !s.contains(&i))
    }
    Some(_) => false,
}
```

The `Package` arm consults `initialized` and is precise. The `Nil` arm is unconditional, and it is what
makes rows 07, 17 and 18 wrong: a deliberately stored `Nil` is read as an absent slot.

**What that arm still catches.** A survey of the fill sites finds exactly five that deposit
`Value::NIL` instead of the `Package("Any")` marker, none of which mark `initialized`:
`src/vm/vm_var_assign_computed_attr.rs:36` (attribute-held arrays),
`src/vm/vm_var_assign_index_named.rs:749` and `src/vm/vm_var_assign_post_incdec.rs:524`
(`__mutsu_array_storage`, the Buf/Blob-style instance storage),
`src/vm/vm_var_assign_post_incdec.rs:612`, plus `coerce_to_hash`'s odd-trailing-key insert
(`src/runtime/utils/coerce_containers.rs:314`/`:349`). Converting those five to the standard
marker-plus-`initialized` pair is the whole prerequisite for deleting the arm — a bounded, enumerable
job, not an open-ended audit. That is the single most important scoping fact in this ADR.

**The read side pays for the collision too.** `resolve_hash_entry` (`src/vm/vm_var_ops.rs:70-87`)
returns `Value::NIL` for a missing key and surfaces no `Option`, so ~15-20 call sites re-test with
`is_nil()` and substitute the container default: `src/vm/vm_var_index_ops.rs:667`, `:673`, `:691`,
`:697`, `:887`, `:1062`, `:1069`, `:1084`, `:1131`, and the array-side `src/vm/vm_var_ops.rs:139`/`:145`
(which additionally rewrite an in-range `Package("Any")` and a shaped in-range `Nil` to the default —
the *inverse* confusion). Each is right for a missing entry and silently wrong for an entry that
genuinely holds `Nil`. Two sites avoid the ambiguity correctly by gating on `map.contains_key` first
(`src/vm/vm_call_method_mut_ops.rs:1731-1747`, `src/runtime/methods_subscript_protocol.rs:56-62`) —
which is the pattern that *should* have been possible everywhere. `AT-KEY`
(`src/vm/vm_call_method_mut_ops.rs:1550-1553`) has no compensation at all, which is rows 25-26.

The result is four readers of one value giving three answers: `{a=>Nil}<a>.WHAT` is `(Any)` (compensated
read), `.values.head.WHAT` and `.pairs.head.value.WHAT` are `Nil` (uncompensated), and `eqv` is `False`
(reads raw storage by construction).

**And there are three divergent hole predicates**, not one: `ArrayData::hole_at`; an open-coded copy in
`trim_trailing_array_holes` (`src/vm/vm_var_delete_ops.rs:60-73`, which also consults the *name-keyed*
type constraint rather than `data.value_type`); and an `initialized`-blind variant in
`src/runtime/builtins_multidim_ops.rs:415-419` (`!is_nil() && !Package("Any")`). A fourth, weaker one
is embedded in the parameterized `Array[T].new(:shape(...))` path
(`src/runtime/methods_object_dispatch_new.rs:1289-1345`), which rebuilds through `array_with_kind` at
`:1327` and thereby **loses the `shaped_array_unassigned` marker**, so every cell of such an array
reports `:exists` `True`.

### 1.7 The compensators already grown next to the gap

Three layers, in three subsystems, for one missing store-side rule:

- **Nine `nil_elems_to_any` rewrites** — `src/vm/vm_var_assign_set_local.rs:946`/`:989`,
  `src/vm/vm_var_assign_local.rs:149`/`:168`, `src/vm/vm_helpers_lazy.rs:608`/`:872`,
  `src/vm/vm_call_method_mut_ops.rs:2768`/`:2780`/`:2796`. All hardcode `Any`, so every call site has to
  gate itself to untyped arrays; none can serve a typed or `is default(...)` container.
- **Two renderer compensators** — `raku_value_as_element` (`src/builtins/methods_0arg/raku_repr.rs:444-452`)
  returns the string `"Any"` for a `Nil` element, and `gist.rs:152-179` does the same for `.gist`
  (`"(Any)"`). Both gate on `kind.is_real_array()`. `raku_repr`'s comment states the rule correctly
  ("a real array element is a Scalar container, so it can never actually hold Nil") — which is precisely
  an admission that the store violated it. These are why `[Nil].raku` *prints* `[Any]` while
  `[Nil][0].WHAT` says `Nil`: the rendering is right and the value is wrong. They reach only real-array
  elements, which is why rows 11-13 (`.sort`/`.reverse`/`.flat`, whose results are `Seq`s) still leak,
  and **there is no equivalent compensation for hash values at all** (rows 20-24).
- **~15-20 read-side `is_nil()` → default sites** (§1.6), which are absent-key handling doing double
  duty.

This is ADR-0040 §1.5's pathology, on a different axis and one layer deeper: a compensator per reader,
each incomplete on a different edge, and the set is not closed.

### 1.8 Why there is no test pressure on this

`roast/S02-types/nil.t` is whitelisted and passes 67/67 — and contains **no** container-element case at
all (its only `@`/`%` mention is `throws-like { Nil.push }`). Nothing in `roast/` or `t/` exercises
`[Nil]`. The rule is pinned only for the paths that already work: `t/typed-array-hole-adverbs.t:81-83`
pins I7/I8 (`my @n = 1, Nil, 3`) and `t/nil-list-holes.t` pins I1/I4. So the gap survived because the
one construct that exercises it has never been tested — not because anything depends on the current
behaviour.

---

## 2. Decision

**When a value is stored into an element of a real, mutable `Array` or `Hash`, a `Nil` becomes that
container's own default (`is default(...)` value → native element zero → element type object → `Any`,
i.e. exactly `Interpreter::typed_container_default`). This applies at construction as well as at
assignment, and per container as it is built. Element reads are not changed, and `List`/`Seq`/slurpy/
`Capture` element stores are not touched. Once the rule holds, `Nil` stops being a hole sentinel:
`ArrayData::initialized` becomes the sole hole discriminator, and the read-side and render-side
compensators are deleted.**

Five parts:

1. **Where.** Every path that puts a value into an element of a real `Array` (`ArrayKind::Array`,
   `Shaped`) or a `Hash`: literal construction (`[…]`, `{…}`, `%(…)`), `Array.new`/`Hash.new` including
   the parameterized forms, list-assign construction (`coerce_to_array`,
   `build_hash_from_items_with_key_coercion`, `coerce_to_hash`), element assign, autovivification, and
   `push`/`unshift`/`append`/`prepend`/`splice`. **Applied at each container as it is constructed** —
   §1.5 (row 29) is the pin that this is per-container and not retro-applied by an outer assignment.

2. **What it decays to.** `Interpreter::typed_container_default` (`src/vm/vm_var_ops.rs:377-410`), which
   already implements the whole ladder: `is default(...)` first, then a native element zero, then the
   declared element type object, then `Any`; it also already handles object hashes with an
   unconstrained value type and user `Array`/`Hash` subclasses. `nil_elems_to_any`'s hardcoded `Any` is
   retired in favour of it, and the ~20 hand-rolled ladders of §1.2 collapse onto it.

3. **What is *not* touched.** `List`, `Seq`, `Range`, `Capture`, `Match`, slurpy `*@x` binding, and
   immutable `Set`/`Bag`/`Mix`. Their elements are not containers, so a stored `Nil` is a real value
   there (I1-I3). `.List` materializing an array hole as `Nil` (I4) is likewise unchanged — it is a
   *hole-to-value* projection, the opposite direction from a store, and once §2.4 holds it becomes
   *correct* for the first time (row 18).

4. **`initialized` becomes the sole hole discriminator.** Delete `hole_at`'s
   `Some(ValueView::Nil) => true` arm (`src/value/value_collections.rs:271`), after converting the five
   `Value::NIL` fill sites enumerated in §1.6 to the standard `Package("Any")`-plus-`initialized` pair.
   Fold the three divergent open-coded hole predicates (`vm_var_delete_ops.rs:60-73`,
   `builtins_multidim_ops.rs:415-419`, and the marker-losing rebuild at
   `methods_object_dispatch_new.rs:1327`) onto `hole_at`.

5. **Delete the compensators.** Both renderers' `Nil` shortcuts, the nine `nil_elems_to_any` call sites
   and the helper itself, and — as far as each is genuinely absent-key-only — the read-side
   `is_nil()`→default sites, preferring the `contains_key` gating pattern that
   `src/vm/vm_call_method_mut_ops.rs:1731` already demonstrates. The read-side set is the delicate half
   (§5.2).

### Why this direction

- **It is the only direction that closes.** The rule is a property of *what a container may hold*.
  Enforced at the store, it holds for `.head`, `.sort`, `.map`, `eqv`, `:exists`, `.raku`, `.List`,
  `AT-KEY`, and sub binding, and for every method added tomorrow, with no per-reader work — rows 09-18
  and 22-26 all fall out of rows 02-03 being fixed. Enforced at the read, it must be re-derived at every
  producer, and §1.7 is the measured evidence that the re-derivation is already three layers deep and
  still incomplete. Row 19 (the dropped element) is not reachable from the read side at all.
- **It converts a four-way sentinel collision into an invariant.** "No element of a real container is
  `Nil`" is checkable, and once true it *retires* mechanism rather than adding it: one deleted `hole_at`
  arm fixes twelve call sites, three duplicate hole predicates collapse onto one, ~20 hand-rolled
  default ladders collapse onto `typed_container_default`, and the `initialized` bitmap that already
  exists becomes load-bearing instead of redundant. Under the project's gain/risk definitions this is
  the archetype of a gain — band-aids removed and a dual mechanism unified.
- **It fixes two type-check divergences in opposite directions with one rule** (rows 27 and 28). Neither
  is reachable by a read-side patch, because both happen at a store.
- **It reuses two shipped primitives** — `typed_container_default` and `initialized` — and adds no new
  `Value` variant, no representation change, and no NaN-box change.
- **It is ADR-0040's decision on a second consequence of the same premise.** Both say "the element is a
  container, so put the property at the store". Landing them on a shared understanding of the store
  sites is cheaper than landing either alone; §5.4 covers the ordering.

---

## 3. Options considered

| Option | Fixes reads (03-18, 22-26) | Fixes stores (19-21, 24, 27-29) | Fixes `:exists`/`.List` (07, 17, 18) | Retires mechanism | Verdict |
| --- | --- | --- | --- | --- | --- |
| **Status quo** | ✗ | ✗ | ✗ | — | Rejected — the defect, and already self-inconsistent across four readers of one value (§1.6) |
| **A. Widen the existing `@`-assignment fixup** (drop the `!is_bind` / `var_type_constraint` gates, add the anonymous-literal case) | partial | partial | ✗ | ✗ | **Rejected.** It cannot reach anonymous construction (`[Nil]` passed straight to a sub, row 05) because it is keyed on a *variable name*; it cannot get row 29 right because it applies the outer container's default (§1.5); it cannot recover row 19's dropped element; and it leaves the sentinel collision entirely. It is also gated on `var_type_constraint(name)`, the very side table ADR-0042 retires. |
| **B. Decay at the element read chokepoints** | partial | ✗ | ✗ | ✗ | **Rejected, with in-tree evidence.** This is what the ~15-20 `is_nil()`→default sites already are, and they produce three answers for one value (§1.6). `.values`/`.pairs`/`.sort`/`.clone`/`AT-KEY`/`eqv` do not pass through any of them (rows 11-14, 22-26), and `eqv` reads raw storage by construction. It cannot fix `:exists`, because the ambiguity it would have to resolve is the one it creates. |
| **C. Compensate in the renderers** | ✗ (renders only) | ✗ | ✗ | ✗ | **Rejected — already tried, twice.** `raku_value_as_element` and `gist.rs:152-179` are exactly this, and they are why `[Nil].raku` prints `[Any]` while `[Nil][0].WHAT` says `Nil`: they make the divergence *harder to find* without fixing anything. |
| **D. Decay at the element store (this ADR)** | ✓ | ✓ | ✓ | ✓ | **Chosen** |
| **E. Make `Nil` unrepresentable in `ArrayData` by construction** (a distinct storage type for element slots) | ✓ | ✓ | ✓ | ✓✓ | Deferred, not rejected. It is the type-level version of D and would make the invariant unbreakable rather than merely maintained, but it changes `ArrayData`'s element type — touching the ~1215 `ValueView::Array(` match sites ADR-0040 §3 counted, plus every `items_mut()` writer. D reaches the same observable semantics without that, and D's slice enumeration is the honest first estimate of what E would cost. |

---

## 4. Phasing

Each slice is independently landable and independently green.

0. **Slice 0 — pin the semantics.** `t/nil-element-store-decay.t`, covering every row of §1.3 as a
   currently-failing expectation set (`todo`-marked so it lands green) **plus every invariant of §1.4 as
   a live assertion**. The invariant half is what stops later slices from "fixing" the divergence by
   purging `Nil` from lists or from hole materialization. Name the regression net explicitly:
   `t/nil-list-holes.t` and `t/typed-array-hole-adverbs.t` pin the hole model and must stay green
   *unchanged*; `t/nil-any-identity.t`, `t/is-eqv.t`, `t/array-slice-oob.t`, `t/pair-subscript-exists.t`,
   `t/uninit-scalar-any.t`, `t/typecheck-expected-nil.t`, and `t/shared-var-nil-redeclared-mask.t` are
   the most likely to encode today's answer and may need re-baselining against raku. Each such edit is a
   finding to call out in the PR, not a licence to edit freely.

1. **Slice 1 — the dropped element.** `exec_make_array_no_flatten_op`'s `ValueView::Nil => {}`
   (`src/vm/vm_data_ops.rs:160`) becomes a decay instead of a drop. Row 19 turns green. This is first
   because it is a one-line data-loss fix that does not depend on any other slice, and because "a `Nil`
   in list context contributes nothing" is a *`List`* rule being misapplied to a real-array builder.

2. **Slice 2 — construction.** `exec_make_array_op` (`src/vm/vm_data_ops.rs:4`, already doing a
   per-element `match val.view()`, so the hook is marginal-cost-free), `exec_make_hash_op` /
   `exec_make_hash_from_pairs_op` (`:168`/`:190`), `build_hash_from_items_with_key_coercion`
   (`src/runtime/utils/coerce_containers.rs:256`, whose three `map.insert` sites at `:281`/`:316`/`:341`
   are the narrowest common store), `coerce_to_hash` (`:14`), `try_native_array_construct` /
   `try_native_hash_construct` (`src/runtime/methods_aggregate_ctor.rs`), and the two parameterized
   `.new` re-implementations (`src/runtime/methods_object_dispatch_new.rs:1289`/`:1384`). Rows 02-08,
   16-17, 20-24 and 29 turn green.

   **Note the ordering constraint from row 29:** the decay uses the container being constructed *right
   now*, so a nested literal decays inside-out. That is automatic if the hook is inside each
   construction op, and wrong if it is hoisted to an outer assignment.

3. **Slice 3 — retire the narrow fixups and unify the ladders.** Replace the two `nil_elems_to_any`
   assignment fixups (`src/vm/vm_var_assign_set_local.rs:974-991`,
   `src/vm/vm_var_assign_local.rs:156-170`) and the `vm_helpers_lazy.rs` / `vm_call_method_mut_ops.rs`
   call sites with a shared `typed_container_default`-based store hook, drop the
   `var_type_constraint(name)` gate, and delete `nil_elems_to_any`. Then collapse the ~20 hand-rolled
   default ladders of §1.2 (`vm_var_assign_index_named.rs:691`, `vm_data_push_ops.rs:9`,
   `vm_var_assign_typed.rs:322`/`:425`, `methods_mut_dispatch.rs:718`,
   `methods_mut_method_lvalue.rs:851`/`:926`/`:1226`/`:1423`) onto the same helper. Rows 27 and 28 turn
   green.

   Row 27 flipping from silent success to a *death* is the single most visible behaviour change in this
   ADR and the thing slice 0 must have baselined first. It is correct (raku dies), but it will surface
   as new failures in any test or battery that builds a typed array from a `Nil`-bearing literal.

4. **Slice 4 — the mutation sites.** Element assign (hook once at the top of
   `exec_index_assign_expr_named_op`, `src/vm/vm_var_assign_element.rs:409`, plus the two pre-dispatch
   fast paths `try_shared_array_element_assign` `:98` and `try_fast_hash_element_assign` `:181`),
   autovivification, and `push`/`unshift`/`append`/`prepend`/`splice` (VM fast path `exec_array_push_op`,
   `src/vm/vm_data_push_ops.rs:45`; slow path `src/runtime/methods_mut_dispatch.rs:732`/`:744`/`:778`/
   `:811`/`:940`). These mostly already behave correctly (I10-I11); the slice makes them share the one
   rule so `is default(...)` and native-zero cases are uniform. **The hook goes inside the per-element
   loop, after each site's own flattening decision** — `push(1,2)` must still add two elements
   (`t/append-one-arg-rule.t` is the pin), exactly as ADR-0040 §2.3 requires for the same sites.

5. **Slice 5 — retire the sentinel.** Convert the five `Value::NIL` fill sites of §1.6
   (`vm_var_assign_computed_attr.rs:36`, `vm_var_assign_index_named.rs:749`,
   `vm_var_assign_post_incdec.rs:524`/`:612`, `coerce_to_hash`'s odd-key insert) to the standard
   `Package("Any")`-plus-`initialized` pair; delete `hole_at`'s `Some(ValueView::Nil) => true` arm; fold
   the three divergent open-coded hole predicates onto `hole_at`; fix the parameterized shaped `.new`
   marker loss (`methods_object_dispatch_new.rs:1327`); and audit the read-side `is_nil()`→default sites
   (§5.2), converting each pure absent-key test to a `contains_key`/`Option` gate. Rows 07, 17, 18, 25
   and 26 turn green. `t/typed-array-hole-adverbs.t` and `t/nil-list-holes.t` are this slice's
   acceptance oracle.

6. **Slice 6 — sweep and close out.** Delete both renderer `Nil` shortcuts
   (`src/builtins/methods_0arg/raku_repr.rs:451`, `src/runtime/utils/gist.rs:152-179`), re-run §1.3 and
   §1.4, delegate the full `make roast` to CI, run the batteries gate (§5.5), record the outcome in this
   ADR's "Implementation status", and `git mv todo/deep/array-literal-nil-not-decayed-at-construction.md`
   to `news/2026-08/`.

---

## 5. Open questions (the forks for the deciders)

1. **Are the five `Value::NIL` fill sites the complete set?** This gates slice 5. The survey behind §1.6
   enumerated them by reading every `autoviv_resize` / `resize` call in the assignment opcodes, and the
   picture is coherent (`Package("Any")` is the marker everywhere else, `initialized` is maintained
   type-blindly). What it did not do is *prove* completeness by execution. *Recommendation: before
   deleting the `hole_at` arm, add a temporary debug assertion in `hole_at` that fires when the `Nil`
   arm is taken, run `make test` + `make roast` under it, and convert whatever it catches. This is
   cheaper and more conclusive than any amount of further reading, and it is exactly the kind of
   invariant a deterministic test suite can prove.*

2. **Which of the ~15-20 read-side `is_nil()` sites are pure sentinel disambiguation?** Some are
   certainly not: `(ValueView::Nil, _) => Value::NIL` (`src/vm/vm_var_index_ops.rs:735`) implements
   raku's real "subscripting `Nil` yields `Nil`" chaining rule and must stay. The array-side rewrites at
   `src/vm/vm_var_ops.rs:139`/`:145` are a *different* bug in the same family (they replace a genuinely
   stored `Any` with the container default) and deserve their own probe rather than being swept in.
   *Recommendation: slice 5 keeps every site it cannot positively classify; leaving a redundant-but-
   harmless default lookup costs nothing, whereas removing a real rule is a regression. The right
   long-term shape is to give `resolve_hash_entry` an `Option` return so absence is expressible, and
   that is a natural follow-up ticket rather than part of this ADR.*

3. **The typed gate and ADR-0042.** Today's fixup is gated on
   `loan_env!(self, var_type_constraint(name))`, the name-keyed side table
   [ADR-0042](0042-type-constraints-belong-to-the-container-not-to-a-name.md) decides to retire. Slice 3
   deletes that gate outright (the container's own metadata answers the question via
   `typed_container_default`), so this ADR *reduces* ADR-0042's surface rather than competing with it.
   `trim_trailing_array_holes` (`src/vm/vm_var_delete_ops.rs:60-73`) is a second consumer of the same
   side table that slice 5 folds onto `hole_at`, removing another. *Recommendation: no ordering
   constraint; whichever lands first, the other's job shrinks. ADR-0042 slice 1 is in flight as
   PR #6743 — re-read it before writing slice 3.*

4. **Ordering against ADR-0040.** Slices 2 and 4 here touch exactly the store sites ADR-0040's slices 1
   and 2 touch, and both want a hook in the same per-element loop. They compose cleanly — "itemize, and
   decay `Nil` to the default" is one transformation applied at one point — but doing them independently
   means enumerating the same ~20 store sites twice. *Recommendation: land whichever is ready first, and
   have the second reuse the first's hook rather than adding a parallel one. If both are unstarted, this
   ADR's slices 1-2 are the cheaper opener: array/hash literal construction is a handful of opcodes and
   two builders, versus ADR-0040 slice 2's much wider `coerce_to_array` caller set.* One interaction to
   check: `Value::item()` on a decayed `Any` type object is a no-op, and `typed_container_default` is
   only reached for `Nil`, so the two hooks are order-independent at a single site.

5. **Does anything depend on a stored `Nil`?** The thirteen invariants in §1.4 found nothing, and there
   is no roast or `t/` coverage of `[Nil]` at all (§1.8). The residual risk is in serializers and
   batteries that round-trip `Nil` through a container: `JSON::Fast`, `CBOR::Simple`, `is-deeply`,
   precompilation serialization, and `nqp::` ops. RSV (`from-rsv`) is the module that surfaced the
   finding and genuinely produces `Nil` elements in normal decode. *Recommendation: slices 1-2 land
   behind the slice-0 pin and the batteries gate; a leaked decay shows up as a deterministic wrong
   `.WHAT`/`.raku`, not as a flake.*

6. **Perf.** The hook is a per-element `is_nil()` check on paths that already do a per-element
   `match val.view()` (`exec_make_array_op`) or a per-element `insert` (the hash builders), so the
   marginal cost should be nil. The one path to watch is `coerce_to_array`
   (`src/runtime/utils/coerce_containers.rs:371-409`), which today *shares* the `Gc` when it finds
   nothing to rewrite; a `Nil` hit forces the rebuild. That is the same risk ADR-0040 §5.2 records for
   the same function, and the same mitigation applies (scan-then-rebuild-only-if-hit, which
   `nil_elems_to_any` already implements at `:356`). *Recommendation: no bench gate for slices 1-2; if a
   later slice touches `coerce_to_array`, reuse ADR-0040 §5.2's measurement.*

---

## 6. Consequences

- **`[Nil]` becomes `[Any]` everywhere at once**, including through `eqv`, `:exists`, `.List`, `.head`,
  `.map`, `.sort`, `.values`, `.pairs`, `.clone`, `AT-KEY`, and sub binding, with no change to any of
  those readers.
- **A silently dropped element is recovered.** `[Nil,].elems` stops being `0`.
- **Two type-check divergences flip in opposite directions.** `my Int @a = [Nil]` starts dying (correct)
  and `my %h{Int} = 1 => Nil` stops dying (correct). The first is the visible one and will surface in
  any code that builds a typed array from a `Nil`-bearing literal.
- **`Nil` stops being a hole sentinel**, so a deliberately stored `Nil` and an absent slot become
  distinguishable — which is what makes `[Nil,1][0]:exists` answerable at all, and what makes `.List`
  correct on a real element for the first time.
- **Mechanism is retired, not added**: `nil_elems_to_any` and its nine call sites, both renderer
  shortcuts, one `hole_at` arm, three duplicate hole predicates, ~20 hand-rolled default ladders, and as
  many read-side `is_nil()` defaults as slice 5 can positively classify.
- **`.raku`/`.gist` output does not change** for the already-compensated real-array case, but
  `.sort`/`.reverse`/`.flat`/`.List` output *does* — `(Nil, 1).Seq` becomes `(Any, 1).Seq`. That is the
  correct raku answer and worth stating in `news/`.
- **If rejected / indefinitely deferred:** the 29 divergences in §1.3 stay; `my @n = 1, Nil, 3` and
  `[Nil, 1]` keep answering `:exists` differently for the same Raku construct; `[Nil,]` keeps losing its
  element; a stored `Nil` stays indistinguishable from a missing entry, so every new element reader must
  re-derive the disambiguation; and the next module that decodes to `Nil` elements rediscovers this as
  an unattributable `is-deeply` failure, exactly as RSV did.

---

## 7. Adjacent findings — the same Raku model, different surfaces

"An `Array`/`Hash` element is a `Scalar` container" is one premise with several observable consequences,
and mutsu approximates each separately. Recorded so a future reader sees the whole shape:

- **This ADR — the *value-domain* surface.** A container cannot hold `Nil`; storing `Nil` restores the
  default. Fixed by decaying at the store.
- **[ADR-0040](0040-array-hash-elements-are-itemized-at-the-store.md) — the *representation* surface.**
  An element is one item in list context and renders `$`-itemized. Fixed by itemizing at the store.
  Same premise, same sites, disjoint transformation.
- **[ADR-0036](0036-element-container-pairs-from-subscripts-and-pairs.md) — the *aliasing* surface.**
  `(@a[0]:p).value` must be the element's container. Fixed by `array_slot_ref`/`hash_slot_ref`
  promotion.
- **`todo/deep/for-loop-rw-element-alias-lost-through-deferred-closure.md` — the *binding-lifetime*
  surface.** Same primitive as ADR-0036, different consumer.

A fifth item is adjacent but not part of this model: `resolve_hash_entry` returning `Value::NIL` rather
than an `Option` (§5.2) is a *plumbing* defect that this ADR makes survivable by guaranteeing no real
element is `Nil`, but does not itself fix. Giving that function an `Option` return is the natural
follow-up ticket once slice 5 lands.

---

## 8. Implementation status (2026-08-20)

Slices 0-2 landed:

- **Slice 0** (acceptance oracle): `t/nil-element-store-decay.t`, pinning all 29 §1.3 rows plus all 13
  §1.4 invariants (16 assertions, since I13 bundles four checks) -- 45 assertions total, dual-oracled
  against `raku` while writing the file. Two rows are `todo`-marked and stay open past this PR (see
  below); every other row and invariant is a live, non-`todo` assertion.
- **Slice 1** (the dropped element): `exec_make_array_no_flatten_op`'s `ValueView::Nil => {}` drop
  (`src/vm/vm_data_ops.rs`) is gone -- a bare `Nil` is now a real element that decays like any other,
  fixing row 19 (`[Nil,].elems` is `1`, not a silently-dropped `0`).
- **Slice 2** (construction): a single new `Interpreter::decay_nil_container_elements` helper
  (`src/vm/vm_data_ops.rs`) calls `typed_container_default` once per freshly-built container and
  rewrites any `Nil` element/value in place (a no-op when the default comes back `Nil`, i.e. a `List`
  or other non-container). Wired into `exec_make_array_op`, `exec_make_array_no_flatten_op`,
  `exec_make_hash_op`, `exec_make_hash_from_pairs_op` (`src/vm/vm_data_ops.rs`),
  `try_native_array_construct`'s and `try_native_hash_construct`'s untyped/typed return paths
  (`src/runtime/methods_aggregate_ctor.rs`, called after `tag_container_metadata` so the decay target
  is typed), and the two parameterized `Array[T].new`/`Hash[V,K].new` re-implementations
  (`src/runtime/methods_object_dispatch_new.rs`). `build_hash_from_items_with_key_coercion` and
  `coerce_to_hash` (`src/runtime/utils/coerce_containers.rs`) are free functions with no `&mut self`
  to call `typed_container_default` from; since neither ever sees a pre-existing typed/`is default(...)`
  container at the point a bare literal is built, a small `decay_nil_hash_value` helper hardcodes the
  same `Any` answer `typed_container_default` would compute there, applied only at genuine
  pair-value inserts (not at the separate odd-trailing-key-with-no-value fallback, which is a
  pre-existing, out-of-scope divergence from raku's actual "Odd number of elements" die). The shaped
  (`:shape(...)`) branches of the two `Array`-constructing functions were deliberately left untouched
  in this slice, to avoid interacting with the shaped-array hole/`initialized` tracking; no acceptance
  row exercises a shaped-array `Nil` element.
- **Bonus, not required by the plan:** rows 27 and 28 (the two type-check divergences) turned out to
  already flip correctly as a side effect of slices 1-2, with no slice 3 needed. `[Nil]`'s own
  construction now decays to `[Any]` *before* `my Int @a = [Nil]` ever assigns it, so the existing
  (unmodified) element type check at the assignment site sees a plain `Any`/`Int` mismatch and dies --
  the same message raku produces. Symmetrically, `my %h{Int} = 1 => Nil` no longer dies, because the
  `Nil` *value* decays to the hash's `Any` value-type default before any check runs (the `Int`
  constraint there is on the key, which was never the problem).
- **Two rows stay open**, both `todo`-marked in the slice-0 test with a comment pointing at their
  territory:
  - **Row 25** (`%h.AT-KEY("missing")` on a genuinely absent key still returns raw `Value::NIL`) is
    slice 5's job (§2 part 5, §5.2) -- `AT-KEY` has no missing-key compensation at all yet. (Row 26,
    the same method on a key holding a *decayed* value, already passes: the map genuinely contains
    `Any` now, so no compensation is even needed.)
  - **Row 29** (`my @d is default(42) = [Nil]; @d[0]` reads back `42` instead of `Any`) is not slice 3
    or slice 5's territory as originally scoped -- measurement found the culprit is
    `resolve_array_entry`'s read chokepoint (`src/vm/vm_var_ops.rs`), which unconditionally substitutes
    a non-`Nil` container default for *any* in-range `Package("Any")` element, without consulting
    `ArrayData::initialized`. That is exactly the "different bug in the same family... deserve[ing] its
    own probe" §5.2 already flagged for the array-side rewrites at `vm_var_ops.rs:139`/`:145` (these
    are those two lines) -- confirmed by direct measurement rather than assumed, so it is recorded here
    as a dedicated follow-up rather than folded into slice 3 or 5.
- **Verification**: `cargo clippy -- -D warnings` and `cargo fmt --check` clean; full local `make test`
  and the regression pins ADR §4 slice 0 named (`t/nil-list-holes.t`, `t/typed-array-hole-adverbs.t`,
  `t/nil-any-identity.t`, `t/is-eqv.t`, `t/array-slice-oob.t`, `t/pair-subscript-exists.t`,
  `t/uninit-scalar-any.t`, `t/typecheck-expected-nil.t`, `t/shared-var-nil-redeclared-mask.t`) all pass
  unchanged; no whitelisted roast file references a `Nil` array/hash literal at all (confirming §1.8).
- **Slice 3** (retire the narrow assignment-site fixups and unify the ladders): the two `nil_elems_to_any`
  list-assign fixups (`vm_var_assign_set_local.rs`, `vm_var_assign_local.rs`) and the three
  `vm_helpers_lazy.rs`/`vm_call_method_mut_ops.rs` groups (five more call sites, nine total) are gone,
  along with `nil_elems_to_any` itself (`src/runtime/utils/coerce_containers.rs`). A new module,
  `src/vm/vm_var_assign_nil_decay.rs`, replaces them with three shared helpers:
  - `Interpreter::assign_store_nil_default(name, container)` -- the store-time default for a whole-
    container (list-)assign, computed BEFORE the container's own type metadata is necessarily tagged
    onto it (`tag_container_metadata`/`coerce_typed_container_assignment` run later in the same opcode).
    It tries the container's own already-embedded state first (the same two checks
    `typed_container_default` itself starts with: an explicit `is default(...)` value, then declared
    element-type metadata), and only when the container carries neither falls back to the *target
    variable's* own declared `is default(...)` / element-type constraint -- the same ADR-0042 side
    table this opcode is about to embed as the container's metadata a few lines later anyway, so
    consulting it here does not reintroduce the retired mechanism, it just answers the question earlier.
  - `Interpreter::decay_nil_elements_for_var_assign(name, value)` -- the whole-array-value fixup itself,
    built on `assign_store_nil_default`.
  - `Interpreter::decay_nil_vec_elements(items)` -- the `Vec<Value>`-based counterpart for call sites
    that build a raw item list rather than an already-tagged container (lazy-list array-context
    reification, and the untyped-only `push`/`append`/`unshift` fast path in
    `vm_call_method_mut_ops.rs`, which bails to the slow path for any typed/metadata-tagged target
    before reaching this call). It reuses `decay_nil_container_elements` (the slice 2 helper) via a
    throwaway untyped-array wrapper instead of re-deriving "untyped real Array defaults to `Any`" a
    third time.

  **The `var_type_constraint(name)` gate is dropped**, as ADR-0042 (§5 open question 3) anticipated: the
  fixup used to skip typed arrays entirely (leaving their `Nil` elements for the separate, still-present
  per-element ladder in `coerce_typed_array_elements`/`coerce_typed_container_assignment`,
  `vm_var_assign_typed.rs:322`/`:425`, to handle downstream). It now decays a typed declaration's `Nil`
  elements too, to the *same* value that downstream ladder would otherwise have produced (verified by
  direct comparison of the two computations) -- so by the time `coerce_typed_array_elements` runs,
  `item.is_nil()` is already false for every element this fixup touched, and that ladder's own `Nil`
  branch becomes correctly unreachable for this caller while staying necessary, unchanged, for its OTHER
  callers (write-through reassignment through a shared cell, and shaped-array sub-array recursion, which
  do not go through the new fixup). This is a genuine, measured "no regression" outcome, not an
  assumption: `my Int @a = 1, Nil, 3` still produces `Array[Int].new(1, Int, 3)` (I12), a bound
  write-through (`my Int @a; my @b := @a; @b = 1, Nil, 3`) still produces the same, and
  `my Int:D @a = 1, Nil, 3` still dies -- with a message that now says `expected Int:D but got Int (Int)`
  instead of the old `expected Int:D but got Nil (Nil)`, which is a strict *improvement*: real raku's own
  message is `expected Int:D but got Int (Int) (perhaps Nil was assigned to a :D which had no default?)`,
  confirmed by direct comparison with `raku -e`. Row 27/28 stay green (they already turned green in
  slice 2, per the "Bonus" note above; this slice's acceptance bar was correctly "no regression", not
  "make it die").

  The other named hand-rolled ladders (`vm_var_assign_index_named.rs:691`, `vm_data_push_ops.rs:9`,
  `methods_mut_dispatch.rs:718`, `methods_mut_method_lvalue.rs:851`/`:926`/`:1226`/`:1423`, and the
  `coerce_typed_array_elements`/`coerce_typed_container_assignment` pair itself) are deliberately left
  in place for this slice: each was measured to already be correct and independently exercised by other
  callers this slice's two fixups do not cover (typed-array element assign with its own type-check
  ladder, `push`/`unshift`'s per-call arity/native-fill handling, write-through reassignment, shaped
  recursion, `:D`-definiteness death). Collapsing them onto `assign_store_nil_default` as well is left
  as a smaller, separately-landable follow-up rather than forced into this slice at the risk of losing
  a working, subtly different piece of logic (e.g. the definiteness death message) for a purely
  cosmetic unification.

  **Verification**: `cargo clippy -- -D warnings` and `cargo fmt --check` clean; the full local `make
  test` (including the slice-0 regression net named above) and a targeted roast sweep of every
  whitelisted `S02-types`/`S09-typed-arrays` array/hash/nil file (19 files, 4795 subtests) all pass;
  `t/nil-element-store-decay.t` rows 27/28 stay live (non-`todo`) and green -- rows 25 and 29 stay
  `todo`, unchanged, deferred to slice 5 as originally planned.

- **Slice 4** (the mutation sites): unified the element-assign ladder
  (`vm_var_assign_index_named.rs:684-707`, the two `__mutsu_array_storage` inc/dec and index-assign
  fill sites in `vm_var_assign_post_incdec.rs`/`vm_var_assign_index_named.rs`) and the
  push/append/unshift/prepend/splice mutators onto the shared `assign_store_nil_default` (element
  assign) / `typed_container_default`-derived helper (mutators), replacing hand-rolled ladders that
  either ignored `is default(...)` or reordered its priority relative to the container's own embedded
  state.

  The element-assign ladder used to check the target NAME's `var_default`/`var_type_constraint` before
  the container's own embedded `is default(...)`/type metadata -- the reverse of ADR-0042's decision
  that a container's own metadata should win. Switching to `assign_store_nil_default`'s
  container-first order fixes a real, verified divergence: `my @a is default(42) = 1,2,3; my @b := @a;
  @b[1] = Nil; @b[1]` now correctly reads `42` (the container's own default, reached through the bound
  alias `@b`, which has no name-keyed entry of its own) -- confirmed identical to `raku -e`.

  `push_nil_to_elem_default` (`vm_data_push_ops.rs`) only ever consulted the declared element TYPE
  (`element_constraint_for`), never a container's `is default(...)` value -- `my @a is default(42) =
  1,2,3; @a.push(Nil); @a.raku` stored `[1, 2, 3, Any]` where raku (and now mutsu) stores
  `[1, 2, 3, 42]`. The same gap existed in the shared `nil_to_elem_default` closure
  `methods_mut_dispatch.rs`'s slow-path `push`/`append`/`unshift` arms share; `prepend`'s arm was
  additionally missing the call to it entirely (a plain `@a.prepend(Nil)` stored a raw, undecayed
  `Nil`). Fixed all four to route through `assign_store_nil_default`.

  A DEEPER bug surfaced while verifying `append`/`unshift`/`prepend` against real `raku`: for a
  single-arg call these never reached the slow path above at all -- `try_native_array_mut`
  (`vm_call_method_mut_ops.rs`) is a VM fast path that bails to the interpreter for any
  typed/metadata-tagged target, but `is default(...)` is a SEPARATE side channel from
  `container_type_metadata`, so an `is default(...)`-only container sailed through the fast path,
  which routes its `Nil` decay through `decay_nil_vec_elements` -- deliberately untyped-only, by design,
  precisely BECAUSE the function's own bail-out guard was believed to already exclude every container
  that needs anything else. Added `self.container_default(target).is_some()` to that guard so an
  `is default(...)` container now correctly falls through to the interpreter path that has the fix
  above. `.splice`'s inserted `Nil` args were left decaying to plain `Any` regardless of the
  container's default -- verified against real `raku` that this is intentional, unlike the other four
  mutators: `my @a is default(42); @a.splice(1,0,Nil)` stores `Any` in raku too. `.splice` previously
  stored a raw, undecayed `Nil` there (mutsu-only, since the target array itself was untyped in every
  case tested); fixed the decay without adding the type-check `.splice` still lacks entirely (a
  separate, pre-existing gap, since closed --
  `news/2026-08/splice-insert-not-type-checked.md`; the check now runs on the post-decay values, so
  a `Nil` spliced into a typed array is rejected as the `Any` it becomes).

- **Slice 5** (retire the `Nil` hole sentinel): converted the five `Value::NIL` fill sites named in
  §1.6 to `Package("Any")` (`vm_var_assign_computed_attr.rs`'s `assign_into_computed_target`,
  `vm_var_assign_index_named.rs`'s and `vm_var_assign_post_incdec.rs`'s `__mutsu_array_storage`
  resize fills, and `coerce_to_hash`'s two odd-trailing-key/single-scalar inserts in
  `coerce_containers.rs`) -- each is also now paired with the standard `initialized`-set
  materialize-then-record pattern where the site autoviv-resizes an array (the ADR's own §5.1
  recommendation: fill AND mark, not fill alone).

  A sixth, unnamed `Value::NIL` fill site was found by direct code reading while implementing the
  above and fixed the same way: `.DELETE-POS`'s array implementation
  (`array_delete_pos_value`, `methods_subscript_protocol.rs`) and its multi-dimensional twin
  (`multidim_delete_pos`, `methods.rs`) both wrote a raw `Nil` into the vacated slot as the delete
  marker -- this is the SAME sentinel-collision pattern the ADR's decision retires, just not in the
  §1.6 survey (that survey covered `autoviv_resize`/`resize` fill call sites, not delete-marker
  writes). Both converted to `Package("Any")`, paired with correct `initialized` bookkeeping.

  **Completeness probe (ADR §5 open question 1), run as specified:** a temporary `debug_assert!` was
  added to `hole_at`'s `Some(ValueView::Nil) => true` arm, then the full local `t/` suite (31292
  tests) and a broad roast sweep (`S02-types`, `S09-typed-arrays`, `S09-multidim`, `S32-array`,
  `S32-hash` -- every whitelisted file in each) were run repeatedly under it. It fired three times,
  each pointing at a genuine, independent bug -- not at gaps the §1.6 survey missed, but at
  PRE-EXISTING logic bugs in the hole-tracking machinery that `hole_at`'s old, imprecise `Nil` arm had
  been silently compensating for (an unconditional "yes, hole" answer needs no `initialized`
  bookkeeping to be right, so nothing downstream of it had ever needed to be correct):

  1. `unmark_initialized_indices`'s `collect_usize_indices` helper had no `ValueView::Whatever` arm, so
     a zen/whatever-slice delete (`@a[]:delete`, `@a[*]:delete`) never recorded ANY of the deleted
     indices in the array's `initialized` set. `trim_trailing_array_holes`'s OLD implementation
     happened to still trim correctly by accident -- it treated `initialized == None` as an EMPTY set
     (`unwrap_or_default()`) rather than "every index present" (`hole_at`'s actual, documented, correct
     convention), so every trailing `Package("Any")` slot looked like a hole regardless of tracking.
     Folding `trim_trailing_array_holes` onto `hole_at` (see below) uses the canonical convention and
     exposed the real bug: `@a[]:exists:delete` on `1, 2, 3` left `[Any, Any, Any]` instead of
     emptying to `[]`. Fixed by special-casing `Whatever` in `unmark_initialized_indices` to clear the
     whole `initialized` set (every index was addressed).
  2. The same `collect_usize_indices` helper's scalar fallback only matched `Int`/`Num`, falling back
     to a string-parse of anything else -- silently dropping a `Rat` index (`1.5` in raku source is a
     `Rat`, not a `Num`; `"1.5"` does not parse as a `usize`). `@a[1.5]:delete` on a 2-element array
     therefore also never recorded the deletion, leaving an untrimmed trailing hole
     (`[3, Any]` instead of raku's `[3]`). Fixed by delegating to the same robust `index_to_usize`
     conversion (Int/Num/Rat/FatRat/BigRat, with a finite check) every OTHER multi-dim/array index site
     already uses, instead of re-deriving a narrower one here.
  3. `multidim_delete_pos`'s outer array rebuild (`Value::array_with_kind(Gc::new(ArrayData::new(updated)),
     ...)`) discarded the source array's `value_type`/`initialized` metadata entirely (a fresh
     `ArrayData::new` starts with `initialized = None`), so a genuinely-nested multi-index `.DELETE-POS`
     (`@a.DELETE-POS(1, 2)` on a `[[1,2],[3,4]]`-shaped array) could never mark anything as a hole no
     matter what marker was stored in the leaf. Fixed by threading `value_type` and a properly
     materialized `initialized` set through the rebuild.

  With all three fixed, the probe ran clean across every subsequent local `t/` and roast pass, and the
  temporary `debug_assert!` was removed along with the `Nil` arm it guarded -- `ArrayData::initialized`
  is now the sole hole discriminator, as decided.

  **The three divergent hole predicates** (§1.6): `trim_trailing_array_holes`
  (`vm_var_delete_ops.rs`) folded onto `arr.hole_at(i)` directly, which also fixes the name-keyed
  `var_type_constraint(var_name)` vs. embedded `value_type` divergence the ADR flagged (a bound/aliased
  array's own metadata now answers the question, matching ADR-0042's direction).
  `builtins_multidim_ops.rs:415-419`'s `multidim_exists_adverb_multi` predicate was deliberately left
  alone at the time (recorded as
  `todo/tickets/multidim-exists-adverb-blind-to-initialized-and-typed-holes.md`) because folding it
  onto `hole_at` needed `(ArrayData, index)` context its shared `multidim_collect_leaves` leaf-collector
  did not carry (only the extracted leaf VALUE). **This is now done** (see
  `news/2026-08/multidim-exists-adverb-canonical-hole-predicate.md`):
  `multidim_collect_leaves`'s output grew a per-leaf `is_hole` flag computed from `hole_at` at each
  `Array`-level iteration/index step and threaded through the recursion (including the
  `ContainerRef`/`Scalar` deref arm), consumed by all six original call sites plus two further
  single-coordinate (`!has_multi_indices`) predicates in the same file that had the identical bug
  (`builtin_multidim_subscript_adverb`'s and `builtin_multidim_exists_adverb`'s non-multi branches, via
  a new `multidim_index_with_hole` companion to `multidim_index`) -- closing every open-coded hole
  predicate in `builtins_multidim_ops.rs`, not just the one originally named. Making the read side
  precise surfaced a companion write-side gap: multidim element assignment (`@a[i;j] = v`, both the
  shaped `assign_array_multidim` and the autoviv/non-shaped `multi_dim_assign_scalar`/
  `multi_dim_assign_slice` paths) never recorded the write in `ArrayData::initialized` at all, so even
  the corrected predicate had no accurate data for an explicitly-assigned `Any` at a multidim
  coordinate; fixed alongside (and `multidim_delete`'s matching gap on removal, needed once the read
  side stopped treating every `Package("Any")` as an unconditional hole). The fourth site the ADR
  named, the parameterized shaped `Array[T].new(:shape(...), :data(...))` marker loss at
  `methods_object_dispatch_new.rs:1327`, was measured directly against real `raku` and found to NOT
  reproduce: both `Array[Int].new(:shape(3))` and `Array[Int].new(:shape(3), :data(1,2))` report
  `:exists` `True` for every cell in RAKU ITSELF (not just mutsu), for both a `:data`-seeded and an
  unseeded shaped array. The ADR's claim of a divergence here was stale/incorrect by direct
  measurement (per the project's `trap-todo-and-adr-root-cause-often-wrong` lesson) -- no fix was made,
  and no ticket was filed since there is nothing to fix.

  **Read-side `is_nil()` audit (§5.2, ADR open question 2):** row 25
  (`%h.AT-KEY("missing")` returning the raw sentinel with no compensation) was fixed --
  `vm_call_method_mut_ops.rs`'s `AT-KEY` fast path now substitutes `typed_container_default` when
  `resolve_hash_entry` answers `Nil` for a missing key, mirroring every other hash-key reader in
  `vm_var_index_ops.rs`. Row 29 (`resolve_array_entry`'s in-range-`Any`-substitution bug, already
  root-caused and deliberately deferred in slice 2's status entry above) stays unfixed and `todo`-marked
  in `t/nil-element-store-decay.t`, exactly as previously scoped -- it is a distinct read-chokepoint bug,
  not a sentinel-retirement task. No other read-side `is_nil()` site was converted: per the ADR's own
  recommendation, a site not positively provable as pure absent-key disambiguation was left alone
  (a redundant default lookup costs nothing; removing a real rule is a regression).

  **Verification**: `cargo clippy -- -D warnings` and `cargo fmt --check` clean; `cargo test --lib`
  (868 tests) and the full local `t/` suite (31292 tests across 3353 files, including every named
  regression pin) pass, run repeatedly across the debug-assertion probe rounds and once more after its
  removal; every §1.3/§1.4 row was re-verified directly against `raku -e` one more time in slice 6
  (below) and all but the still-`todo` row 29 agree; a targeted roast sweep of every whitelisted
  `S02-types`, `S09-typed-arrays`, `S09-multidim`/`6.d/S09-multidim`, `S06-signature/multidimensional.t`,
  `S09-subscript/multidim-assignment.t`, `S32-array`, and `S32-hash` file (61 files) passes.

- **Slice 6** (sweep and close out): both renderer `Nil` shortcuts deleted --
  `raku_value_as_element` (`raku_repr.rs`) no longer special-cases `v.is_nil()`, and `gist.rs`'s
  real-array element loop no longer branches on `is_real && v.is_nil()`; both are unreachable dead
  code after slices 1-5 (no real-array element can hold `Nil` any more), confirmed by the same
  verification pass as slice 5 (nothing in `t/`/the roast sweep exercised the removed branches, and
  `.raku`/`.gist` output for every §1.3/§1.4 row is unchanged). §1.3 and §1.4 were re-run one final
  time directly against `raku -e`/mutsu one-liners (not just the pinned test file) as this slice's own
  closing check; every row matches except the still-open row 29. `make roast` is delegated to CI per
  usual policy -- not run locally for the full suite. `todo/deep/array-literal-nil-not-decayed-at-
  construction.md`, the finding this ADR supersedes, is `git mv`'d to `news/2026-08/` as part of this
  same change, rewritten as an accomplishment.

---

*This ADR is Accepted for slices 0-6 (fully implemented). If the mechanism judgment changes for some
future consequence of this decision, supersede rather than rewriting.*
