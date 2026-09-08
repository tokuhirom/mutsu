# A lone `is Array` subclass element gets its trailing comma back

A real array holding a single Iterable element renders a trailing comma so the
`.raku` round-trip does not flatten it — `[[1, 2],]`, `[1..5,]`. mutsu applied
that rule to plain containers but not to an `is Array` / `is Hash` subclass
instance:

```raku
class SA is Array { }
my $c = SA.new(3, 2, 1, 4);
my @h = $c;
say @h.raku;
# raku:  [[3, 2, 1, 4],]
# mutsu: [[3, 2, 1, 4]]      <- was
```

`.elems` and `.^name` agreed on both, and the plain-container controls
(`[[1, 2],].raku`, `my $p = [1,2]; my @q = $p; @q.raku`) were already correct —
so this was purely the outer array's rendering, and only for that one element
shape.

## Root cause

An instance's `.raku` needs interpreter method dispatch, so
`Interpreter::expand_raku_leaves` renders such leaves itself and splices the
resulting text back into the tree as a `raku_raw` placeholder before handing the
tree to the pure renderer. That design is deliberate — its own doc comment says
it exists "so every container keeps its exact bracket / itemization /
trailing-comma rules instead of a duplicated walk having to re-derive them" —
but the placeholder is a `Str`, and a `Str` is not Iterable. By the time
`element_needs_trailing_comma` looked at the element, the one fact it needed had
been erased.

## The fix

The placeholder now carries it. `RAKU_RAW_ITERABLE_KEY` is the twin marker for a
leaf that itself does Iterable, and `expand_container` picks it via
`raku_leaf_is_iterable` — an instance is container-backed exactly when it
carries the `__mutsu_array_storage` / `__mutsu_hash_storage` attribute that every
container method on it delegates to. `element_needs_trailing_comma` recognises
that marker, and also handles a container-backed instance reaching it directly,
for the paths that do not go through dispatch expansion.

`t/array-subclass-element-raku-trailing-comma.t` pins it — including the
non-Iterable instance, the two-element arity control and both plain-container
controls — and passes under rakudo as well as mutsu.

## Two rows split out rather than folded in

The ticket listed two more divergences; both turned out to have different root
causes, and both got their own issue:

- [#7658](https://github.com/tokuhirom/mutsu/issues/7658) — `$d.raku` on a
  scalar holding such an instance drops rakudo's `$` marker. Measurement shows
  this is **not** subclass-specific: a plain `my $f = @b` loses it too, and the
  semantics are already right (`my @z = $f, 3` does not flatten, `$($f).raku`
  renders `$[1, 2]`). It is the scalar-assignment *store* not itemizing, which
  is ADR-0040's territory, not a rendering row.
- [#7660](https://github.com/tokuhirom/mutsu/issues/7660) — an explicit
  `@h.gist` / `@h.Str` on such an array answers `SA()`, the element class's type
  object. The ticket asked for `.gist` to be verified against rakudo before
  touching it; it was, and mutsu is wrong. The implicit `say @h` is correct, so
  it is an explicit-dispatch delegation bug, not a rendering one.

Closes [#7594](https://github.com/tokuhirom/mutsu/issues/7594).
