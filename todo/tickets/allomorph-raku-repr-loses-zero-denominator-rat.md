# `RatStr.new(1/0, "1/0").raku` renders the numeric half as `Inf` instead of `<1/0>`

Found while fixing `todo/tickets/angle-bracket-quoted-word-space-padded-loses-allomorph.md`
(see `news/2026-08/angle-bracket-quote-word-allomorph-whitespace.md`).

## Symptom

An allomorph whose numeric half is a `Rat` with a **zero denominator** renders that half as `Inf`
in `.raku`, losing the exact `<1/0>` spelling:

```raku
say RatStr.new(1/0, "1/0").raku;   # raku: RatStr.new(<1/0>, "1/0")   mutsu: RatStr.new(Inf, "1/0")
say < 1/0 >.raku;                  # raku: RatStr.new(<1/0>, "1/0")   mutsu: RatStr.new(Inf, "1/0")
```

## This is display-only — the value itself is correct

Every other observable is right, so this is confined to the `.raku` rendering path:

```raku
my $x = < 1/0 >;
say $x.^name;         # RatStr      (correct)
say $x.Str;           # 1/0         (correct)
say $x.numerator;     # 1           (correct)
say $x.denominator;   # 0           (correct)
say $x.Rat.raku;      # <1/0>       (correct -- the inner Rat renders fine on its own)
say $x.raku;          # RatStr.new(Inf, "1/0")   <-- wrong
```

Note that `(1/0).raku` is `<1/0>` and `< 1/0 >.Rat.raku` is `<1/0>`, so the bare `Rat` renderer is
fine; something on the `Mixin` path coerces the inner value before rendering it.

## Pre-existing, not caused by the whitespace fix

It reproduces through `RatStr.new(1/0, "1/0")` with no `<...>` syntax involved at all. The
whitespace fix only made it *reachable* from a new expression (`< 1/0 >` is now a `RatStr` rather
than a plain `Rat`, matching raku).

## Affected files (starting point)

- `src/builtins/methods_0arg/raku_repr.rs` — the `ValueView::Mixin(inner, mixins)` arm calls
  `raku_value(inner)` for the numeric half; that call is where the `<1/0>` spelling is lost.

No test currently pins this, and `t/allomorph-angle-bracket-whitespace.t` deliberately asserts the
`.numerator` / `.denominator` / `.^name` facts rather than `.raku` so it does not encode the bug.
