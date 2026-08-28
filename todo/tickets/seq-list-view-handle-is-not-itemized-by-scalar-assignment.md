# A `.cache` List-view handle is not itemized when assigned to a `$` variable

`.raku` on a `List` held in a `Scalar` container renders with the itemization
sigil — rakudo's `List.raku` takes its invocant raw and asks `nqp::iscont`, so
`my $c = (1,2,3); $c.raku` is `$(1, 2, 3)`, not `(1, 2, 3)`. mutsu models the
same thing on the value side: assigning a `List` to a `$`-sigiled variable
converts its `ArrayKind::List` to `ArrayKind::ItemList`, which the `.raku`
renderer prints with the `$` prefix.

The `SeqView::List` handle that `.cache` returns for a **not-yet-reified** `Seq`
(ADR-0038 §2) is not an `Array` at all — it is still a `ValueView::Seq` with a
`List` view tag — so scalar assignment has nothing to re-tag, and the
itemization is silently lost:

```raku
my $d = Seq.new(class :: does Iterator {
    has @!stuff = <a b c>;
    method pull-one { @!stuff and return @!stuff.shift; IterationEnd }
}.new);
my $c = $d.cache;
say $c.raku;
# raku:  $("a", "b", "c")
# mutsu:  ("a", "b", "c")
```

Only the *deferred* arm diverges. `.cache` on an already-reified `Seq` returns a
real `Value::array`, and that one itemizes correctly (`my $c = <a b c>.Seq.cache;
$c.raku` is `$("a", "b", "c")` in both). Without a `Scalar` container the two
agree as well (`<a b c>.Seq.cache.raku` is `("a", "b", "c")` in both) — the
divergence is exactly "value in a scalar container".

## Why it is not just a renderer tweak

The renderer is not where the information is missing: by the time `.raku` runs,
nothing distinguishes "this List-view handle was assigned to a `$`" from "this
one was not". Fixing it means either

* giving `SeqBody`'s handle an itemization bit alongside `SeqView` (so
  `SeqView` becomes `Seq | List | ItemList`, and every site that builds a handle
  has to decide), or
* having scalar assignment *materialise* a still-deferred List-view handle into
  a real itemized `Value::array` — which would force the body and defeat the
  whole point of ADR-0038 §1.6 (an infinite `:as`/`lines` source must survive
  `.cache`), or
* deferring the decision: keep the handle, but let the scalar-assignment path
  record itemization on the handle and have the renderer consult it.

The third is probably right, but it is an ADR-0038 amendment, not a one-liner.

## Blast radius / priority

Cosmetic today: it is visible only through `.raku`, and `.raku` of a `.cache`d
deferred `Seq` held in a scalar is not something any whitelisted roast file or
`t/` test asserts. It surfaced while writing
`t/seq-cache-list-view-and-eqv-routine.t` (which sidesteps it by rendering
without a container, and says so in a comment). It would start to matter if a
test ever round-trips such a value through `.raku.EVAL`, since the itemization
is part of the round-trip.

## Minimal repro

```
SNIPPET='my $d = Seq.new(class :: does Iterator {
    has @!stuff = <a b c>;
    method pull-one { @!stuff and return @!stuff.shift; IterationEnd }
}.new); my $c = $d.cache; say $c.raku'

raku  -e "$SNIPPET"    # $("a", "b", "c")
mutsu -e "$SNIPPET"    #  ("a", "b", "c")
```

The `does Iterator` class is required: `Seq.new(<mutsu's own built-in
iterator>)` takes the eager shortcut in `try_native_seq_construct`, so its
`.cache` returns a real `Value::array` and both agree (`$(1, 2, 3)`). Only the
deferred body reaches the List-view handle.

## Affected files

* `src/value/seq_body.rs` — `SeqView`, `SeqBody::as_list_view`
* `src/value/view.rs` — `Value::seq_list_view`, `Value::seq_list_view_as_list`
* `src/builtins/methods_0arg/raku_repr.rs` — the `ValueView::Seq` arm
* wherever scalar assignment re-tags `ArrayKind::List` as `ArrayKind::ItemList`
