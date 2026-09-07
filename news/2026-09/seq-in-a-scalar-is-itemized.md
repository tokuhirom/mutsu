# A `Seq` stored in a `$` scalar is itemized

`my $s = (1, 2).Seq; say $s.raku` printed `(1, 2).Seq` where rakudo prints
`$((1, 2).Seq)`. Every other list-shaped value already rendered its scalar
container — `$(1, 2)` for a List, `$[1, 2]` for an Array, `${:a(1)}` for a
Hash, `$(1..3)` for a Range — so a real `Seq` was the one shape that slipped
through `itemize_scalar_store_value`. The Seq-producing expressions that
surfaced it (`my $z = zip(...)`, `my $m = (1, 2 Z <a b>)`) were the same defect,
not a `zip`/meta-op one.

## Why the container is a handle tag, not a wrapper

The obvious fix — wrap the `Seq` in a `Scalar`, which is what the explicit
`$(...)` path (`itemize_value`) has always done — is wrong, and the existing
`t/dir-seq-map-gist.t` catches it. Rakudo's single-argument rule still
*flattens* a `$`-held `Seq` into a `+@` slurpy:

```raku
sub slurpy(+@v) { @v.elems }
my $s = (1, 2, 3).Seq;  slurpy($s);   # 3
my $l = (1, 2, 3);      slurpy($l);   # 1
my $a = [1, 2, 3];      slurpy($a);   # 1
```

A `Scalar` wrapper hides the `Seq` from that check and collapses the first row
to `1`. So the container is recorded the way `ArrayKind::ItemList` and the
`Hash` itemization flag already record theirs: as a new `SeqView::ItemSeq` tag
on the `Seq` *handle*, sharing the same reification core (ADR-0038 S5). Every
type-facing consumer treats `ItemSeq` exactly as `Seq` — `.^name`, `.WHAT`,
`eqv`, the `Seq ~~` smartmatch, `.sort`'s fresh result — and only `.raku` reads
the tag, matching rakudo's `nqp::iscont(SELF)` check.

Retagging pulls nothing from the source, so a lazy `Seq` stays lazy
(`my $s = lazy gather { ... }` is still `.is-lazy`) and the sink exemption
continues to ride on the shared `SeqBody` (`mark_itemized`), untouched.

## A `Seq`'s identity is its core, not one handle

Retagging exposed a latent assumption: the two interpreter-side tables that
hang off a `Seq` — the `PredictiveIterator` carrier that `Seq.new(iterator)`
fills, and the squish/unique iterator metadata — were keyed on one handle's
`Arc` address, so a second handle over the same sequence found nothing. They
are now keyed on `SeqBody::identity()`, the shared reification core's address,
which every handle over one sequence answers alike. That is what those tables
always meant (they describe the sequence, not the handle), and it makes the
`.cache` List view find them too. Pinned by the existing
`t/predictive-seq-tail-scope.t`.

## Scope

`.raku` was the visible damage; `.elems`, element access, iteration, `.VAR`
(`Scalar`) and list-context arity already agreed with rakudo and are unchanged.
A `:=` bind must not itemize and does not. Pinned by
`t/seq-scalar-store-itemization.t` (26 assertions, all measured against rakudo
2026.07), which covers the repro, the other list shapes as invariants, the bind,
parameter and loop-variable binding, laziness, the sink exemption, the
single-argument rule rows above, and `.raku` round-tripping through `EVAL`.
