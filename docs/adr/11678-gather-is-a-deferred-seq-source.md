# ADR-11678: A gather is a deferred `SeqSource`

- **Status**: Proposed
- **Date**: 2026-10-07
- **Addresses**: [#11678](https://github.com/tokuhirom/mutsu/issues/11678)
- **Related**: [ADR-0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md)
  (the `SeqBody` reification and consumption contract; §2.5 currently leaves `LazyList`
  untouched), [ADR-0058](0058-map-grep-produce-a-deferred-seq.md) (deferred producers live
  in `SeqSource`), [ADR-0119](0119-seq-sources-pulled-a-prefix-at-a-time.md) (pull-granular
  `SeqSource`s)

## 1. Context

`gather { ... }` currently produces a `ValueView::LazyList`. Its coroutine runs when the
`LazyList` is forced, but a `SeqBody` reader cannot invoke the VM: before reification it exposes
an empty seed generation. Consequently a gather nested in a Pair, Hash, or list can be rendered
before anything has forced its body. Forcing at the collection insertion point is not sound:
finiteness is unknown until the body completes, and an infinite gather would hang during
construction. A variable reference can also preserve the lazy value beyond the insertion point.

The current issue's rendering-only reproductions are:

```raku
my $g = gather { take 2 };
say ("a", $g).raku;       # rakudo: ("a", $((2,).Seq))
my %h = a => (gather { take 5 });
say (%h<a>:kv).raku;      # rakudo: ("a", $((5,).Seq))
say $g.raku;              # rakudo: $((2,).Seq)
```

The first two are also wrong in mutsu because the nested deferred body is not reified by the
rendering path. The third additionally lacks the scalar-itemized Seq handle view.

## 2. Proposed decision

Represent a gather as a deferred `SeqBody` source, rather than as a second lazy-sequence
representation. Add a coroutine-backed `SeqSource::Gather` whose source retains the captured
gather body state and whose pulls use the existing VM gather resume machinery. Creation remains
lazy and does not execute the body. `SeqBody` continues to own the shared reification,
consumption, identity, and itemization behavior defined by ADR-0034.

The source must support incremental pulls as well as full reification: a bounded consumer must
not force an unbounded gather, and a later pull must resume the same coroutine and collector.
The source's captured values must participate in `SeqBody` tracing and edge dropping. No Rust
reference may outlive the owning GC handle, and no interpreter is constructed to perform a pull.

When a gather is stored in a scalar, use `SeqView::ItemSeq` (or the equivalent existing handle
path) so `.raku` preserves `$` itemization. Container insertion must retain the Seq as one value;
it must not flatten the deferred producer merely because it is represented by `SeqBody`.

## 3. Consequences

- Pair, Hash, and list rendering reach the same deferred Seq machinery and reify only when their
  reader requires elements.
- Constructing a collection containing an infinite gather stays bounded and does not run the
  gather body.
- Gathers gain the `SeqBody` identity, shared alias, consumption, cache, and itemization rules;
  all differences from the current `LazyList` behavior must be measured against Rakudo.
- This changes ADR-0034 §2.5's explicit exclusion of `LazyList`-backed gathers. If accepted, update
  that ADR's status to point here without rewriting its historical decision.

## 4. Rejected approaches

- **Force every gather found in a collection.** Finiteness cannot be proved before execution;
  this hangs while constructing a collection containing an infinite gather.
- **Add special cases to Pair/Hash/list rendering.** That duplicates the lazy forcing protocol at
  each consumer and leaves aliases and other readers with the same empty seed.
- **Keep `LazyList` and add a renderer-only forcing path.** This preserves two independent
  reification, consumption, and aliasing systems for values that Raku presents as `Seq`.

## 5. Acceptance evidence

Before marking this decision Accepted, implementation and focused oracle comparisons must cover:

1. The three examples in §1, plus the issue's variable-held list and Pair cases.
2. Construction of a list and Pair containing an infinite gather without executing it; a bounded
   prefix read returns only its requested prefix and can resume correctly.
3. Repeated aliases, `.cache`, consuming reads, scalar itemization, `Slip`/flattening, `take` of a
   nested gather, exception/control-flow behavior, and GC tracing of an unforced gather.
4. Existing gather, `Seq`, lazy-list, collection rendering, and roast regressions.

The implementation must use compiler/VM execution paths and must not add collection-specific
rendering hacks or eager forcing.
