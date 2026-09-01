# ADR-0040 slice 3 — `.VAR` on an element answers from the source container

The last `todo`-marked row of ADR-0040's acceptance oracle is green, and the
ADR is now implemented end to end (slices 0-3).

Raku's model is that a real, mutable `Array`/`Hash` stores each element in a
`Scalar` container, while a `List`/`Seq`/`Range` stores the values themselves.
Slices 1 and 2 put the *representation* half of that at the element store: a
stored aggregate is itemized, so it renders `$[1, 2]` and counts as one item in
list context. Slice 3 covers the *reflection* half — `.VAR` — which is the one
place the stored value cannot carry the answer on its own:

```
my @c = 1, (1,2), [3,4];    @c[0].VAR.^name, @c[1].VAR.^name, @c[2].VAR.^name
my @l := 1, (1,2), [3,4];   @l[0].VAR.^name, @l[1].VAR.^name, @l[2].VAR.^name
```

raku answers `Scalar Scalar Scalar` for the first and `Int List Array` for the
second. The two first elements are a byte-identical bare `Int`, so no flag on
the value can distinguish them — the answer has to come from the *source
container's* kind. mutsu answered `Scalar` for both.

## What changed

`Value::elements_are_containers` (`src/value/value_methods_a.rs`) states the
discriminator once, beside the itemization helpers the earlier slices added:
`Array`/`Shaped`/`Lazy`/`ItemArray` and every `Hash` have container elements;
`List`/`ItemList`, `Seq`, `Range` and everything else do not; a `Scalar` wrapper
recurses into what it holds, so a `List` living in a `$` answers from the List.

Its consumer is `builtin_index_var_meta` (`src/runtime/builtins.rs`), the
runtime half of the compiler's `.VAR`-on-a-subscript rewrite. That function used
to synthesize an opaque `Scalar` descriptor unconditionally — its one
pre-existing exception being `Map`, whose values are famously not containers,
which is exactly the rule this slice generalizes.

The compiler hook was **rewired rather than the builtin taught to index**. It
used to compile the subscript's target purely for side effects, throw the result
away, and pass `(name, index)` so the builtin could re-derive a `Map` value by
key. Re-deriving the element inside a reflection builtin is a hand-rolled
duplicate of the subscript machinery — the compensator-per-site shape this ADR
exists to avoid, and one that would have needed its own arms for negative
indices, `Seq` reification and `Range` arithmetic. The hook now compiles the
whole subscript and passes `(element, name)`, so the element is read once by the
ordinary machinery and the builtin only decides which of the two to hand back.
`Seq`- and `Range`-sourced elements then fell out for free.

One representation ambiguity needed the variable's sigil: mutsu's `LazyList` is
the reified form of both a real `Array` assigned a lazy source (`my @a = ^Inf`)
and a lazy `Seq` (`my $s = lazy gather {…}`), which raku answers differently.
The sigil resolves it soundly rather than heuristically — raku rejects binding a
`Seq` to an `@` variable outright, so an `@`-sigiled lazy list can only have got
there by assignment.

Fixed in the same function: `@real[0].VAR.name` and `%h<a>.VAR.name` are
`@real` / `%h` in raku (Rakudo names an element's container after the container
it lives in), where mutsu synthesized `@real[]` / `%h[]`.

Unlike slices 1 and 2, this one had no counter-currents. Their recurring trap
was a reader asking a question *about the value* while holding something
itemized *because it is an element* — 17 such sites in slice 2 alone. Slice 3
changes no stored value and no flattening decision.

## What is left

`.VAR` on a *real* element still returns an opaque descriptor carrying
`name`/`dynamic`/`default`, where raku returns the element's actual container
and delegates value methods through it (`@real[1].VAR.raku` is `[3, 4]`, not
`Scalar.new`; `.VAR.elems` is `2`, not `1`). Fixing that means `.VAR` returning
ADR-0036's `ContainerRef` element cell instead of a descriptor — the *aliasing*
surface, and a representation decision of its own. Recorded with three smaller
siblings found in the same measurement (the `@a[i;j]` multi-dim subscript never
reaching this path, `is default(0)` not reflected in `.VAR.default`, and native
`int @a` elements needing an `IntPosRef`) as
`todo/deep/var-on-a-real-element-is-an-opaque-descriptor-not-the-container.md`.
