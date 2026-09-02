# ADR-0064: A `.VAR` container descriptor carries the value its container holds

- Status: Accepted (implemented)
- Date: 2026-09-02
- Related: ADR-0040 (array/hash elements are itemized at the store),
  ADR-0057 (`.VAR` reflection identity is the shared cell's address),
  ADR-0036 (element container pairs from subscripts)
- Addresses: `todo/deep/var-on-a-real-element-is-an-opaque-descriptor-not-the-container.md`

## Context

`.VAR` in Raku hands back the *container* — the real `Scalar` a mutable
`Array`/`Hash` element lives in, or the `Array`/`Hash` an `@`/`%` variable
names. A container is transparent for ordinary method dispatch: everything
that is not a property of the container itself is answered by the value it
holds.

mutsu instead synthesized an attribute-only reflection `Instance` (class
`Scalar`/`Array`/`Hash`/`Sub`) carrying `name`, `dynamic`, `default`, `of` and
an identity, and *nothing else*. Every method Raku answers from the contained
value was therefore answered out of an empty attribute map:

| program | raku | mutsu (before) |
| --- | --- | --- |
| `@real[1].VAR.raku` | `[3, 4]` | `Scalar.new` |
| `@real[1].VAR.gist` | `$[3, 4]` | `Scalar.new` |
| `@real[1].VAR.elems` | `2` | `1` |
| `@real[1].VAR[0]` | `[3 4]` | `Nil` |
| `%h<a>.VAR.raku` | `[1, 2]` | `Scalar.new` |
| `$z.VAR.raku` (`my $z = [3,4]`) | `[3, 4]` | `Scalar.new` |
| `@real.VAR.raku` | `[1, [3, 4]]` | `Array.new` |
| `%d<a><b>.VAR.^name` (`%d<a><b>=1`) | `Scalar` | `Int` |

The originating ticket framed the honest fix as "return the element's real
`ContainerRef` cell instead of a descriptor", and noted why that is not a
patch: the read chokepoint deliberately decontainerizes a `ContainerRef` on
every element read, and the descriptor carries information a cell cannot
(`.name` and `.dynamic` are properties of the *variable*, not of the element).

## Decision

Keep the descriptor as the representation, and give it the one thing it was
missing: **the value its container holds**. The descriptor then answers its
own properties and delegates everything else.

1. **Every `.VAR` descriptor records its content** under `__mutsu_var_value`.
   - An **element** descriptor (`@a[0].VAR`, built by
     `builtin_index_var_meta`) records the element. Nothing else can find it
     again: `__mutsu_var_target` names the *container* the element lives in.
   - A **variable** descriptor (`$x.VAR`) records the variable's shared
     `ContainerRef` cell when it has one — then reads through the descriptor
     dereference it and stay live — and otherwise the value the VM handed
     `.VAR`, refreshed on every `.VAR` call. The refresh is an in-place write
     through the shared attribute cell, so the cached instance keeps the
     identity ADR-0057 gave it and any role mixed into it.
   - The snapshot is deliberately preferred over an env lookup: a plain
     `my $x` in a mainline block is frequently absent from `env` entirely
     (the `locals`/`env` dual store), while the VM-supplied value is
     authoritative at that instant.

2. **The descriptor owns a fixed, small set of methods**
   (`var_meta_owns_method`): `VAR`, `var`, `name`, `dynamic`, `default`,
   `of`, the identity/type reflections (`WHICH`, `WHAT`, `HOW`, `WHO`, `WHY`,
   `WHERE`, `REPR`, `DEFINITE`, `isa`, `does`, `self`), every metamethod
   (`.^*`) and every private call. `defined` is owned too — a container object
   is always concrete, so `my @a; @a[0].VAR.defined` is `True` even though the
   element is `Any`.

3. **Everything else delegates to the contained value**, with two
   container-aware spellings that fall straight out of ADR-0040's itemization
   (the recorded value is already in its itemized form):
   - `.gist` shows the **container**, i.e. the itemized value's `.raku`
     (`@a[1].VAR.gist` is `$[3, 4]`, not `[3 4]`);
   - `.raku`/`.perl` show the **contained value**, i.e. decontainerize first
     (`@a[1].VAR.raku` is `[3, 4]`).

   Both are `Scalar`-only. An `@`/`%` descriptor *is* the container, so
   `@a.VAR.gist`/`.raku` are just the Array's own.

4. **A `Scalar` is not `Positional`**, so subscripting a descriptor follows the
   one-item rule every non-positional value obeys — `@a[1].VAR[0]` is the
   container's content, `@a[1].VAR[1]` is an `X::OutOfRange` over `0..0` —
   rather than subscripting one level too deep into the content.

5. **A container stringifies as its content at the Value level too.** Raku's
   `is [1,2,3][1].VAR, 2` passes because binding the container to a parameter
   decontainerizes it; mutsu's `Test` builtins and `~` reach
   `Value::to_string_value()` directly, which would otherwise render the
   descriptor `Instance`. One arm in `value/display.rs` applies the same rule
   there.

6. **The native fast paths defer.** `native_method_0arg` returns `None` for a
   descriptor on any non-owned method (`var_meta_descriptor_defers`), because
   only the interpreter can resolve the contained value. `.VAR` is rare, so
   this costs nothing measurable on the hot dispatch path.

### A subscript's parent does not need a NAME

The descriptor is built by `builtin_index_var_meta` from the *source
container*, which the compiler names. A chained subscript (`%d<a><b>`,
`@g[0][1]`) has no name to give: its parent is the intermediate value `%d<a>`,
which lives under no variable. Raku still answers from that parent —
`[1,2][0].VAR` is `Scalar` while `(1,2)[0].VAR` is `Int`, and `@a[1][0].VAR` is
`Scalar` or `Int` depending on whether `@a[1]` is an `Array` or a `List` — so
the discriminator is the same one, applied to a value rather than to a name.

So the compiler puts the parent on the **stack** next to the element instead of
naming it: `<parent>; Dup; <index>; Index`, then
`__mutsu_anon_index_var_meta(parent, element)`. `Dup` is what keeps this to ONE
evaluation of the parent expression, which matters as soon as it has side
effects (`bump()[0].VAR`). Everything the descriptor would have read from the
variable degrades to what raku reports for an anonymous container: `.name` is
`element`, `.dynamic` is `False`, `.default` is `(Any)`, `.of` is `(Mu)`.

Two subscript shapes are excluded, because their own compile paths would be
bypassed by that manual emission: a `PseudoStash` subscript (`CALLER::<$x>`,
which compiles to `GetCallerVar`) and a `%*ENV<k>` one (`GetEnvIndex`). Neither
is a real container whose elements would answer `Scalar` anyway.

### The four element-`.VAR` spellings the descriptor now reaches

- **Multi-dimensional subscripts.** `@sh[0;0]` reaches the compiler as
  `Expr::MultiDimIndex`, not `Expr::Index`, so `.VAR` never entered the
  element path at all. Both spellings now route through
  `var_on_index_source_name`. A shaped array's element containers are
  *anonymous* in Rakudo (its storage is a separate dimensioned repr), so their
  `.name` is `element`, not `@sh`.
- **`is default` and `of`.** An element's container inherits the declared
  container's default and element type, read exactly as the variable path
  reads them (value-carried default first, so it survives binds and rebuilds).
- **Native arrays.** `my int @a` stores unboxed values, so its elements have
  no `Scalar` of their own: Raku hands back a per-element-type positional ref
  (`IntPosRef`/`UIntPosRef`/`NumPosRef`/`StrPosRef`), a plain `Any` subclass
  with none of `Scalar`'s container properties. Dropping the `name`/`of`/
  `default`/`dynamic` attributes is what makes those methods fail there: they
  are descriptor-owned, so an absent attribute falls through to ordinary
  dispatch.
- **Slices.** `@a[0,1]` hands back a `List` *of* containers, and `.VAR` on a
  `List` is identity — Raku answers `List`, not `Scalar`. Two gates cover this
  between them. The compiler's `index_expr_is_slice` catches the statically
  recognizable spellings (a comma list, a range, an `@`-sigiled index) and
  compiles them as ordinary subscripts. Everything else — `@a[*]`, and an index
  whose *runtime* value happens to be a list — is caught by a value-side
  discriminator in the descriptor builder, and only because of ADR-0040: a real
  container itemizes every element it stores, so a bare, non-itemized `List`
  arriving there never came out of one element slot (an element holding a list
  reads back as `$(1, 2)`, a slice as `(1, 2)`). The compile-time gate stays
  because it is also the only one that works for a `LazyList`-backed container,
  where itemization is still incomplete.

## Consequences

- Every row of the originating ticket's table now matches raku, as do all the
  siblings it split off — including the multi-dim/chained one the ticket had
  re-measured as "bigger than the original note said". Pinned by
  `t/var-container-descriptor.t`, whose 62 assertions pass under real `raku` as
  well as under mutsu.
- ADR-0040's itemization becomes load-bearing for *reflection*, not just for
  rendering and flattening: the slice/element discriminator is derived from it.
  That promptly exposed a gap in it — a lazy source assigned to an `@`
  variable reified bare elements — which the discriminator carved out for one
  day before the gap itself was closed
  (`news/2026-09/lazy-array-elements-are-itemized-at-the-force.md`); the
  carve-out is gone.

### Accepted residual divergences

- **`.Str` on a `Scalar` descriptor** delegates (`@a[1].VAR.Str` is `3 4`)
  where Rakudo resolves it on `Scalar`'s own MRO and answers `Mu.Str`
  (`Array<0x...>`). mutsu's `Mu.Str` renders as `C()` rather than
  `C<addr>` anyway, so matching Rakudo here would trade one divergence for
  another; the delegated answer is the useful one. Same for the handful of
  other methods Rakudo resolves on the container type and then fails
  (`.Numeric`, and arithmetic through it, which Rakudo cannot resolve at all).
- **An unresolved method** on a descriptor reports the *contained* value's type
  (`No such method 'nosuch' for invocant of type 'Int'`) rather than `Scalar`.
- **`.name` on a native positional ref** returns `Nil` instead of throwing:
  mutsu answers `.name` generically for any `Instance` from its attribute map.
  `.of`/`.default`/`.dynamic` do throw.

### Not decided here

Making `.VAR` return the element's live `ContainerRef` cell — so that a
descriptor held across an assignment to *another* alias of the same element
tracks it — remains ADR-0036's aliasing surface. The variable path already
gets that behaviour for free when the variable is boxed; the element path
snapshots.
