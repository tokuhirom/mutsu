# Collapse the five copy-pasted `.cache` LazyList arms into one helper

Phase 4 of [ADR-0038](../../docs/adr/0038-seq-cache-returns-a-list-and-the-seq-list-view-is-a-property-of-the-value.md)
(the deliberately-deferred cleanup pass) is done. ADR-0038 S1.7/S1.8 found that
the `.cache` `LazyList` arm — `ll.is_genuinely_lazy() || ll.is_cat_pull()` ->
`with_cached_no_sink().with_list_context()` — was copy-pasted at five call
sites (`src/builtins/methods_0arg/collection.rs`,
`src/runtime/methods_call_dispatch.rs`, `src/vm/vm_call_method_ops.rs`,
`src/vm/vm_call_method_mut_ops.rs`, `src/vm/vm_native_dispatch.rs`), and that
the three `__mutsu_lazylist_*` markers behind it
(`__mutsu_lazylist_list_context`, `__mutsu_lazylist_cached_no_sink`, and the
array-context marker) lived as stringly-keyed entries in the value's captured
closure `env` rather than typed `LazyList` fields.

Both are fixed, as a pure refactor with no intended behaviour change:

- `LazyList` gained three typed bool fields — `array_context`, `list_context`,
  `cached_no_sink` — parallel to the `SeqView` field ADR-0038 added to
  `SeqBody`. All of the existing accessor/mutator methods
  (`in_array_context`/`with_array_context`, `in_list_context`/
  `with_list_context`, `is_cached_no_sink`/`with_cached_no_sink`) keep their
  same names and signatures, so every call site outside `value_lazy.rs`
  (~20 of them) needed no change — only their implementation moved from an
  `env.get`/`env.insert` string lookup to a direct field read/write.
- A new `LazyList::cache_lazy_view(&self) -> Option<Value>` method captures
  the whole `.cache` arm (the guard and the tagged-clone construction) in one
  place. All five call sites now call it; the `vm_native_dispatch.rs` site
  (structurally different — it's one arm of a `lazy_pipe_preserving_coercion`
  match) uses `.unwrap_or_else(|| target.clone())` since its guard
  (`lazy_pipe.is_some()`) is already a subset of the helper's own guard.
- Adding the three fields required touching every direct `LazyList { .. }`
  struct literal in the codebase (nine of them, split between simple
  all-`false` constructions and two "manual clone with a modified `env`"
  sites that now copy the three fields from the source list) plus the seven
  `new_*` constructors and the hand-written `Clone` impl in `value_lazy.rs`.
- `value_lazy.rs` had already drifted past the repo's 500-line-per-file
  convention (584 lines) before this change; splitting it further pushed it
  to 603. Took the opportunity to split the seven `new_*` constructors plus
  the scan-reduction forcer (`force_scan_to`/`scan_binary_op`) out into a new
  `src/value/value_lazy_ctors.rs`, leaving `value_lazy.rs` with the
  `Debug`/`Clone` impls and the accessor/mutator/`cache_lazy_view` methods.
  Both files are now under 500 lines.

Verified with `t/seq-cache-returns-list.t` (the ADR-0038 oracle-agreement
pin) plus the full `t/seq-*.t`, `t/lazy-seq-*.t`, `t/*lazy*.t`, and
`t/io-cathandle-lazy.t` suites, `cargo clippy -- -D warnings`, and a
before/after comparison of the three `MUTSU_REAL_TEST=1` roast files ADR-0038
phase 3 fixed (`roast/S16-io/words.t`, `roast/S32-io/io-cathandle.t`,
`roast/S32-list/tail.t`) confirming no crash and identical pass/fail counts
before and after this refactor.
