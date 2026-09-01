# `.pairs` hands out element containers, and the leak that blocked it was one general bug

`my @a = <A B>; my $p = @a.pairs[0]; @a[0] = "Q"; $p.value` is `Q` in raku,
because an `Array`/`Hash` element is a `Scalar` container and `.pairs` hands out
*that container* rather than a snapshot of what it held. mutsu now does the same:
`.pairs` joins `.values`/`.reverse`/`.sort`/`.kv` on the container-aware producer
layer (`src/vm/vm_element_producers.rs`), which closes rows 3, 4 and 9 of
[ADR-0036](../../docs/adr/0036-element-container-pairs-from-subscripts-and-pairs.md)
§1.3 and finishes slice 3.

## Why it was blocked, and why it isn't any more

`.pairs` was implemented, measured and **backed out** on 2026-08-27. Handing out a
Pair whose *value* is a cell leaks into every consumer that reads a pair's value
**as data**, and because `.pairs` promotes the source's elements in place, the
exposure is not "consumers of the `.pairs` result" but "consumers of any container
a producer has ever run over". Five distinct leaks were measured then, and the
conclusion drawn was that the pattern would not stop: `set_coerce.rs` and
`coerce_containers.rs` alone destructure a pair's value structurally in 15 places,
and across `src/` roughly 460 sites bind a `ValueView::Pair`/`ValuePair` value.
The finding was filed as a deep ticket asking for a new read-boundary decision —
a distinct "pair value view" that `.VAR` consumes, or an audited conversion of
every structural read.

**Re-measured on 2026-09-01, four of the five leaks were already gone.** Not
because of anything `.pairs` did, but because the pair work that landed in between
closed them: §1.3 row 10 (a FatArrow's `Index` RHS captures the element container),
row 11 (the standalone-pair env rebind stopped faking writes through an immutable
pair value), row 12 (the promoted cell carries the container's element type),
`.WHAT` on a `ContainerRef` answering the value's type instead of `Scalar`, and
the `.kv` multi-parameter raw bind from ADR-0045 slice 5. With `.pairs` routed,
the **entire** `t/` suite (3597 files) passed unchanged, and a full local
`make roast` reduced to **one** failing file.

That one file — `roast/S03-metaops/infix.t`, 396/5076 subtests — was not a
`.pairs` bug at all. `%a = %reset.pairs` into a `BagHash`/`MixHash` collapsed every
weight to `1`, because `pair_weight` and `mix_pair_weight`
(`src/builtins/quanthash_coerce.rs`) matched on the pair value's view without
decontainerizing: a cell missed every numeric arm and fell through to the truthy
`_` fallback. **That was already wrong on `main`**, with no `.pairs` involved — a
plain `key => $x` pair has carried a container since row 10 landed, so

```raku
my $x = 3; my %z is BagHash; %z = ((a => $x),);   # BagHash(a)  -- should be BagHash(a(3))
```

was broken before this change. Reading through the container at those three
functions fixes both the pre-existing bug and the `.pairs` regression.

## The other half: storing a pair value is a copy

Routing `.pairs` also made a second pre-existing gap easy to hit. `.value` has to
return the *container* — ADR-0036 row 6 requires `(@a[0]:p).value.VAR.^name` to be
`Scalar` — and mutsu's read chokepoints are variable reads and element reads, not
method returns. So a `.value` result reaching a **store** arrived as a live cell,
and four store sites kept it instead of copying:

```raku
my %h = a => 1; my @l; @l.push(%h.pairs[0].value); %h<a> = 9;  # [9], raku: [1]
```

In raku only a *bind* aliases; `push`, `append`, an array element assign and a
hash element assign all store a copy. The fix reads through the container at each
of those four, all of which already had a normalization hook to hang it on
(`normalize_push_unshift_arg`, `flatten_append_args`, the element-assign
itemization hook in `vm_var_assign_element.rs`, and the slow-path element assign);
the `:=` bind arrives wrapped in a `__mutsu_bind_index_value` marker and is
untouched, and the reference-push shape (`@a.push(@b)`, which deliberately shares
a cell) is skipped by its own `value_source_idx` guard. Like the weight bug, all
four reproduce on `main` with a plain `key => $x` pair and no `.pairs` in sight.

## The rule this establishes

**A Pair's value is read as data everywhere except an lvalue `.value =` and
`.VAR`.** Any site that *type-tests* or *numifies* a pair's value must
`deref_container()` first; a site that merely passes it along does not, because
the list and element chokepoints already decontainerize. That is the same
asymmetry `.antipairs` lives under (a pair's *key* is never a container) and the
reason `.values`/`.reverse`/`.sort` never needed the rule: they hand out a flat
list of cells, and list consumers decontainerize. It is specifically the Pair
wrapper that carries a cell into code that reads it structurally.

The deep ticket's premise — that this needed a new read-boundary design and an
audit of hundreds of sites — turned out to be wrong, but only measurably so: the
audit it feared was replaced by a full-suite sweep, and the sweep found one
function, plus four store sites the ticket never named. The general lesson is
the one the ticket itself half-stated: a "pattern that did not stop" was really
several separate defects, and fixing most of them elsewhere left the shared
chokepoints visible.

## What is pinned

- `t/pairs-element-container.t` (new, 37 tests, every expectation cross-checked
  against real `raku`): the container itself (live reads, write-through, `.VAR`,
  invisibility to `.raku`/`.elems`), immutable sources keeping the snapshot
  producer (`List`, `Set`, a mutable QuantHash's weight arm, a shaped array), and
  the consumer side — Hash-from-pairs copying rather than aliasing, Bag/Mix/
  BagHash/MixHash weights, `.map({.key => .value})`, `.antipairs`, `.invert`,
  hyper `.value`, `trans` with a closure replacement, and the typed-array
  constraint. The container-valued-Pair weight case is pinned *without* `.pairs`,
  since that is where the bug actually lived. Four more pin that `push`, `append`
  and both flavours of element assign store a copy of a pair value.
- `t/subscript-pair-element-container.t` rows 3, 4 and 9 lose their `todo`
  markers, as does the `.pairs` `.VAR.^name` probe.

Verified with the full `t/` suite, a full local `make roast` (required by the
"universal property of values" rule — this changes what is inside a promoted
container), and the bundled-battery gate (274/297, unchanged).
