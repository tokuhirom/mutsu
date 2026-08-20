# `Seq.cache` now returns a `List`, fixing four `is-deeply` stack overflows

`Seq.cache` must return a `List` per the rakudo contract — in mutsu it sometimes
returned a value that still bound `Seq:D`. The vendored upstream
`Test.rakumod`'s `is-deeply` relies on that narrowing as the termination
condition for its `multi sub is-deeply(Seq:D $got, Mu $expected, ...)`
candidates (`is-deeply $got.cache, $expected, $reason`): when `.cache` fails to
narrow, the same candidate re-selects itself forever and the real Rust stack
overflows (`SIGABRT`). This was the largest shared mechanism left in the
real-`Test` campaign, affecting four files under `MUTSU_REAL_TEST=1`:
`roast/S16-io/words.t`, `roast/S32-io/io-cathandle.t`,
`roast/S32-list/tail.t`, and `t/io-cathandle-lazy.t`.

Root-caused and designed in
[ADR-0038](../../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md),
which found two independent defects behind the one symptom:

- **Facet A** — a *deferred* `SeqBody` (`Seq.new($iterator)`,
  `IO::Handle.lines`/`.words`) had nowhere to record "this handle is a `List`
  view of that body": `.cache` returned the receiver unchanged, so both type
  oracles kept answering `Seq`. Fixed by giving `SeqBody` a `SeqView` tag
  (`Seq` | `List`) that belongs to the *handle*, not the shared reification
  state — `SeqBody` was split into a per-handle `view` field plus an inner
  `Arc<SeqCore>` (the `gens`/`state` machinery) that a `.cache` result shares
  with the original Seq. `Value::seq_list_view(body)` builds that second
  handle without pulling or cloning any elements, so `.cache` stays
  non-forcing on a genuinely infinite source (measured against `raku`:
  `.cache.^name` answers `List` before anything is pulled) while `retained`/
  `Taken` transitions made through either handle stay visible through both.
- **Facet B** — for a cat-pull `LazyList` (`IO::CatHandle.lines`/`.handles`),
  the `.cache` List view already existed, but two type oracles disagreed:
  `value_type_name` checked the list-context marker before `is_cat_pull()`,
  while `type_matches_value`'s hot-path `tag_match` fast-accept keyed off
  `is_cat_pull()` alone and never consulted the marker, so multi-dispatch kept
  binding `Seq:D` against a value whose `.^name` was already `List`. Fixed by
  making `tag_match` defer to `value_type_name` for `ValueView::LazyList`
  instead of carrying its own copy, and deleting a third, independently
  drifted copy of the same table in `methods_introspect.rs` (whose default
  arm answered `Seq` where the authoritative table answered `Array` — fixing
  that surfaced a real pre-existing bug where an untagged genuinely-lazy Seq's
  `.WHAT.^name` disagreed with its `.^name`; both now correctly answer `Seq`,
  matching `raku`).

All four originally-crashing files now run to completion (no more `SIGABRT`);
remaining subtest failures in them are a separate, already-known issue (`eqv`
on a Seq of different origin than its comparison List, explicitly out of scope
per ADR-0038 S6). A generated matrix test,
[t/seq-cache-returns-list.t](../../t/seq-cache-returns-list.t), pins
`.cache.^name`/`~~ Seq:D`/`~~ List:D` across every flavour from the ADR's
measured table (reified `Seq`, lazy pipe, `gather`, deferred `Seq.new`,
`IO::Handle.lines`/`.words`, `IO::CatHandle.lines`/`.handles`) and matches
`raku` row for row.

Phase 4 of the ADR (collapsing five copy-pasted `.cache` `LazyList` arms and
promoting the `__mutsu_lazylist_*` env magic strings to typed fields) is pure
cleanup with no behaviour change and was deliberately deferred — tracked in
`todo/tickets/collapse-lazylist-cache-copies.md`.
