# ADR-13918: A hyper or race sequence over a lazy source pulls its batches on demand

- **Status**: Proposed
- **Date**: 2026-10-20
- **Deciders**: tokuhirom, Claude
- **Issue**: [#13918](https://github.com/tokuhirom/mutsu/issues/13918)
- **Related**: [ADR-13891](13891-lazy-receivers-pull-before-rows.md) (lazy receivers pull before
  rows; its §6 audit left this difference open), [ADR-0034](0034-seq-reification-is-in-place-and-distinct-from-consumption.md)
  (the `Seq` body and its deferred sources), [ADR-11276](11276-built-in-methods-are-handler-rows.md)
  (`HyperSeq`/`RaceSeq` rows, §9.137)

## 1. Context

`.hyper` and `.race` are in the lazy step's reify table (`method_table::lazy_pull::reifies`), so
an infinite receiver is forced and the force throws `X::Cannot::Lazy`:

```raku
say (1..*).map(*+0).hyper.map(*+1).head(3);   # Rakudo: (2 3 4)   mutsu: Cannot .hyper a lazy list
```

Taking the two names out of the table only moves the failure: `hyperize_with_config` builds the
sequence's body from `value_to_list(target)` (a settled `Vec`), and every reader of a hyper or
race sequence (`hyper_seq_as_list`, `hyper_seq_iterator`, `hyper_seq_map_grep`) copies that
`Vec`. The payload is already a `SeqBody` (ADR-0034), the same body a `Seq` has, and a `SeqBody`
can hold a deferred source (`SeqSource::MapGrep`, `Gather`, `Iterator`, a pure cursor) whose
elements are pulled on demand. Only the hyper/race constructors and readers assume a reified one.

Rakudo's `HyperSeq` wraps the source iterator and a `HyperConfiguration`; its workers pull
`batch` elements at a time and run the stages over each batch, so `.head(3)` pulls a few batches
(measured on Rakudo 2026.09: 17 source elements for `:batch(4)`), never the whole source.
`(1..*).hyper.is-lazy` is `False` there, and `.configuration` reports the adverbs as given.

## 2. Decision

1. **A hyper or race sequence keeps its source deferred.** `.hyper`/`.race` on a receiver the
   lazy step would reify (an infinite or not-yet-pulled lazy list) builds the `HyperSeq`/`RaceSeq`
   over a `SeqBody` whose source is the receiver itself, with the `:batch`/`:degree`
   configuration on the body as today. `hyper` and `race` leave the reify table. A finite,
   settled receiver keeps today's reified body (nothing changes for it).
2. **`map`/`grep` on a deferred hyper or race sequence append a stage, they do not run it.** The
   result is a hyper (race) sequence over a `SeqSource::MapGrep` stage on the previous body, as a
   plain `Seq`'s `map`/`grep` already is. Its configuration is carried over.
3. **A pull from a deferred hyper or race sequence takes whole batches.** The body is pulled
   `batch` source elements at a time (64 when unspecified, as Rakudo); each batch runs through the
   stages on the worker pool exactly as a settled sequence's `map`/`grep` runs today
   (`exec_hyper_race_map_grep`), and the results are appended to the body's generations. A
   `hyper` sequence appends batches in source order; a `race` one may append a batch's results in
   completion order, as today.
4. **Every reader goes through the body's pull, not a `Vec` copy.** `hyper_seq_as_list` and
   `hyper_seq_iterator` become a list or iterator view over the body (the `Seq` machinery's
   `as_list_view` / iterator over generations), so `.head(n)`, `.first`, `for` and `.iterator`
   pull only the batches they read; a method that needs every element (`.elems`, `.List`, `.sort`)
   reifies the body and throws `X::Cannot::Lazy` for an infinite source, as the `Seq` rows do.

## 3. Consequences

- `(1..*).map(*+0).hyper.map(*+1).head(3)` answers `(2 3 4)`, with at most a few batches of the
  source pulled; `.race` likewise (any order, so tests sort).
- A finite hyper/race sequence behaves exactly as before: the reified body is the degenerate
  case of the batched pull (one generation holding everything).
- `HyperSeq`/`RaceSeq` lose their last "settled from birth" assumption, so
  `claim_hyper_iterator_once` (rakudo #4413: one `.iterator` per hyper sequence) keeps working on
  the body flag it already uses.
- The worker pool runs one batch at a time per pull, so a deferred hyper pipeline has at most
  `degree` batches in flight; Rakudo prefetches more. The difference is observable only through
  side effects in the stages (a counter in a `map`), which Rakudo does not specify.

## 4. Alternatives considered

- **Keep the settled `Vec` and throw (status quo).** Wrong answer for a valid program; rejected.
- **Reify a bounded prefix eagerly at `.hyper` time.** No bound is right for every consumer
  (`.head(3)` vs `.first(* > 10_000)`), and a prefix that is too short silently truncates.
- **A new `ValueRepr::LazyHyperSeq` kind.** Doubles every `HyperSeq`/`RaceSeq` match arm and the
  rows' owner hooks for no gain: the `SeqBody` already models "deferred until pulled".
- **Sequential, unbatched pulls for a lazy source.** Simpler, but drops the worker pool for lazy
  pipelines entirely and makes `:batch` meaningless there; batching keeps one code path for
  finite and lazy sources.

## 5. Open questions

- Whether `.hyper` on a *finite* but deferred `Seq` (a `gather` that ends, `lines` of a file)
  should also stay deferred, which saves memory but changes when the source runs. This ADR keeps
  today's behaviour for it (reified at `.hyper`), and decides only the receivers the lazy step
  would otherwise reify.
