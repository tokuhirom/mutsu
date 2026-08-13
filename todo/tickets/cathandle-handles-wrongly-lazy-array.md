# `IO::CatHandle.handles` is wrongly lazy, and wrongly an Array

`IO::CatHandle.handles` should return an eager `Seq` (Rakudo:
`.handles.^name` is `Seq`, `.handles.is-lazy` is `False`), but mutsu builds
it as a genuinely-lazy `LazyList`:

| | raku | mutsu |
| --- | --- | --- |
| `$cat.handles.^name` | `Seq` | `Array` |
| `$cat.handles.is-lazy` | `False` | `True` |

Constructed via `LazyList::new_cat_pull` (`src/value/value_lazy.rs:452`),
used from `src/runtime/native_io/io_cathandle.rs`. Since gap 1 of
`todo/deep/cache-on-a-lazy-seq-must-not-answer-seq.md` (fixed, see
`news/2026-08/lazy-seq-cache-list-name.md`) this no longer crashes anything,
but `t/io-cathandle-lazy.t` still fails 2 of its 9 subtests under
`MUTSU_REAL_TEST=1` (the vendored `Test.rakumod`):

```
not ok 5 - CR-LF is a single line ending
# expected: $("a", "b", "c")
#      got: ("a", "b", "c").Seq
not ok 6 - lazy .handles: reads 2 lines per handle
# expected: $(("a1", "a2"), ("b1", "b2"), ("c1", "c2"))
#      got: (...)
```

Both failures are `is-deeply` comparing an eager expected List against a
value that (a) is untagged (no `in_list_context()`/`in_array_context()`
marker, so it falls through to `value_type_name`'s default
`LazyList(_) => "Array"`) and (b) is lazy when raku's is not — so the real
`is-deeply`'s `eqv`-based comparison sees mismatched shape/type and fails
where raku's Seq-narrowing `is-deeply` candidate (which normalizes both
sides to eager Lists first) would pass.

Under the default (non-`MUTSU_REAL_TEST`) `Test` module the file passes,
because the native `is-deeply` normalizes any `LazyList` via `seq_to_list`
before comparing (`src/runtime/test_functions/comparison.rs:322-341`).

Fix is presumably in `IO::CatHandle`'s `.handles` builder: either construct
it as an eagerly-reified `Seq` up front, or give it the `is_from_gather()`-style
"report as Seq, not lazy" treatment `value_type_name` already has for gather
blocks. Needs investigation of why `.handles` was made lazy in the first
place (likely to avoid opening every constituent handle eagerly) before
picking an approach — it may need to stay a lazy *pull* internally while
answering `Seq`/`is-lazy == False` externally, similar to how gap 1 separated
"stays lazy" from "reports as List".

Found in the vendored-`Test.rakumod` campaign,
`todo/tickets/vendor-real-test-module.md`.
