# Collapse the five copy-pasted `.cache` `LazyList` arms into one helper

**Status: open. Phase 4 of [ADR-0038](../../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md), deliberately deferred when phases 1-3 landed (the ADR marks phase 4 as cleanup with no behaviour change, droppable without reopening the ticket).**

## Background

ADR-0038 fixed the two independent defects that made `Seq.cache` fail to narrow
to `List` (a Rust-level stack overflow in `Test.rakumod`'s `is-deeply`): the
type-oracle disagreement between `value_type_name` and `type_matches_value`
(facet B), and the missing `SeqView` on a deferred `SeqBody` (facet A). Both
are fixed and pinned by `t/seq-cache-returns-list.t`.

ADR-0038 S1.7 also found, while confirming those two, two smaller latent
issues that are NOT part of the crash and were left for a follow-up:

- The `.cache` `LazyList` arm (`ll.is_genuinely_lazy() || ll.is_cat_pull()` ->
  `with_cached_no_sink().with_list_context()`) is copy-pasted at **five**
  sites: `src/builtins/methods_0arg/collection.rs`,
  `src/runtime/methods_call_dispatch.rs`, `src/vm/vm_call_method_ops.rs`,
  `src/vm/vm_call_method_mut_ops.rs`, `src/vm/vm_native_dispatch.rs`.
- The three `__mutsu_lazylist_*` markers (`__mutsu_lazylist_list_context`,
  `__mutsu_lazylist_cached_no_sink`, the array-context marker) are stringly-
  keyed entries in the value's captured closure `env`, not typed `LazyList`
  fields.

## What to do

Per ADR-0038 S4 phase 4: collapse the five `.cache` `LazyList` copies into one
shared helper, and promote the three `__mutsu_lazylist_*` env magic strings to
typed `LazyList` struct fields (parallel to the `SeqView` field this ADR added
to `SeqBody`). No behaviour change is expected; this is purely reducing the
"three homes, three readers" structural finding recorded in ADR-0038 S1.8.

## Why this is a separate ticket

Purely a cleanup/dedup pass with no crash or correctness fix riding on it —
landing it separately from the ADR-0038 crash fix keeps that PR's diff focused
on the two facets it needed to fix.
