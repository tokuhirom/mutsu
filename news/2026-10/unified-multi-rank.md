# Multi subs and multi methods rank by one narrowness relation

Multi-sub and multi-method dispatch used to rank candidates with two
separate pieces of code (#12468). The sub side built a seven-field tuple and
had its own comparator, tie comparison, incomparability prune and
scan-stop check, and the dispatch plan and per-type dispatch program each
repeated the scan-stop check. The method side ran a chain of `retain`
filters (literal count, type distance, narrowness, slurpiness, named bind
check, optional count, owner), with its own copy of the prune that only ran
when every candidate came from the same class.

Both now rank by one `MultiRank` (`src/runtime/multi_rank.rs`). It holds the
tier order (`tier_cmp`), the total order with declaration order
(`total_cmp`), rakudo's parameter-wise `is_narrower` relation, the scan-stop
check and a generic `prune_to_maximal`. The slurpy, named-bind-check and
optional-count helpers take a parameter slice, so the sub and method rankers
call the same functions. The method ranker builds a `MultiRank` per match,
sorts by it and leaves only an owner tie-break (most-derived class, then a
class's own method over a role-composed one) for the candidates that are
still tied.

The method prune now also runs across classes. When an ancestor's candidate
and a subclass's are incomparable (each is narrower on a different
parameter, and one carries a `where`), the one declared first wins. As in
rakudo, the ancestor's candidate counts as declared first, because a
subclass's dispatcher is derived from its parent's. With
`class P { multi method m(Int $x, Any $y) }` and
`class C is P { multi method m(Any $x where True, Int $y) }`, `C.new.m(1, 2)`
now answers `P` as rakudo does. mutsu used to pick `C`.
