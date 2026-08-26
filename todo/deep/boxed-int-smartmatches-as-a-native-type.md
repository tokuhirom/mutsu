# A boxed `Int` smartmatches as a native `int` — and mutsu has no way to tell them apart

The last open item of the four filed as
`todo/tickets/native-type-smiley-and-element-check-gaps.md`; the other three are
closed, see
[`news/2026-08/native-type-smiley-and-element-check-gaps.md`](../../news/2026-08/native-type-smiley-and-element-check-gaps.md).
Re-measured 2026-08-26, and the measurement is why this moved from
`todo/tickets/` to `todo/deep/`: the original ticket assumed a wrong
type-check predicate, and the real answer needs a representation change.

## What the ticket said

```raku
say 5 ~~ int;     # raku: False   mutsu: True
say 5 ~~ uint8;   # raku: False   mutsu: True
```

A boxed `Int` is not a native `int`, and mutsu answers `True` for every native
integer type. `num`/`num32`/`num64` and `str` behave the same way:

```raku
say 1e0 ~~ num64;  # raku: False   mutsu: False   (already right, by accident)
say "x" ~~ str;    # raku: False   mutsu: True
```

## Why "make it False" is the wrong fix

The half the ticket did not measure:

```raku
my int $x = 5;      say $x ~~ int;      # raku: True
my num64 $n = 1e0;  say $n ~~ num64;    # raku: True
```

Rakudo's answer depends on the **representation of the value**, not on anything
about the literal `5`. A native-typed container holds a genuinely native `int`,
and reading it yields a native `int`; a literal `5` in the mainline is a boxed
`Int`. Both reach `~~` as "the number five".

mutsu's `Value` has one `Int` variant and no native/boxed distinction — it is
NaN-boxed, with no spare bit carrying "this came out of a native container". So a
blanket `False` would fix `5 ~~ int` and simultaneously *break* `$x ~~ int`,
trading one divergence for another rather than closing a gap. There is no local
change to the type-check predicate that gets both right.

## What closing this actually needs

A representational way to distinguish a native scalar from its boxed peer at the
point `~~` sees it — either a distinct `Value` variant/tag for native-typed
reads, or having a native-typed container's read site carry its declared type
along. Both are ADR-scale: `Value`'s layout is pinned by NaN-boxing (ADR-0001
layer 3b) and every arithmetic/dispatch site treats `Int` as one thing.

## Priority

Low, and unchanged from the original filing: no bundled battery needs it, and
nothing in roast is known to turn on it. It is recorded so the next person to
reach for the "obvious" one-line fix in `type_matching.rs` sees the counter-case
first. Worth doing only if a native-value representation is being added for
other (performance) reasons anyway, in which case this comes along nearly free.
