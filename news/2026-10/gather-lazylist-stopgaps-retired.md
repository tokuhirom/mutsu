# The gather `LazyList` stopgaps are retired

Before a plain `gather` became a `Seq` (ADR-11678), it was a `LazyList`
coroutine. Two stopgaps from #12366 made one held inside a collection readable:
`.raku` dispatched on such a coroutine at render time (`needs_raku_dispatch`),
and the nested-read pass forced it (`reify_nested_map_grep_for_read`).

Since #14204 a streamed gather leaves its Seq only as a pipe over that Seq, and
nothing else hands the inner coroutine out, so no plain gather reaches either
arm. Both arms and `LazyList::is_gather_coroutine` are deleted (ADR-11678 step
4, #13026). A gather nested in an Array, a List, a Hash or a Pair still renders
its elements, now through the `Seq` arm alone.
