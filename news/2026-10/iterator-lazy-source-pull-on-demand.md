# `.iterator` on a lazy source pulls on demand

`.iterator` on a lazy receiver used to fill the `Iterator` instance with the
receiver's elements up front. For an unbounded Range that was a 1M-element
prefix, so `("a"..*).iterator` took about 0.4 s to build, and `pull-one` past the
cap answered `IterationEnd` as though the infinite source had ended.

Now `build_iterator_instance` gives a lazy receiver an empty prefix and keeps the
source as the instance's `lazy_source`. An unbounded Range is kept as its
`.succ`-stepping LazyList, and a LazyList (a lazy pipe, a `gather`) is kept as
itself. Each protocol method (`pull-one`, `push-exactly`, `skip-*`, `push-all`)
pulls only as far as it needs. So `(1.5..*).iterator`, `("a"..*).iterator` and
`(1..*).map(*+1).iterator` are O(1) to build and have no cap (#10782).

Some consumers read the materialized prefix directly. `List.from-iterator` and
the `PositionalBindFailover` coercion now drain the lazy source. `Seq.new($it)`
over such an iterator becomes a deferred Seq that pulls on demand. Like
Rakudo's `IntRangeUnending` / `SuccFromInf` / gather iterators, an iterator
over a lazy source with no known count has no `count-only` / `bool-only`.

The change also fixed a related bug. After a `gather`-backed iterator had pulled
a partial prefix, a full drain went through the tree-walk bridge. That bridge
treated the partial cache as the whole list, so `push-all` after one `pull-one`
appended nothing. The drain now forces through `force_lazy_list_vm`, which
resumes the gather.
