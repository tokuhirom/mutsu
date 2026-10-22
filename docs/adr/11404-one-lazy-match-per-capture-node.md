# ADR-11404: One lazy Match per capture node

- **Status**: Accepted (user decision 2026-10-22; proposed 2026-10-12). Implemented in the PR
  that references #11404.

## Context

A regex or grammar match is a lazy value (ADR-0016 P5): `ValueRepr::Match(Gc<MatchNode>)`
holding the stored capture node (`Arc<CapNode>`) and the shared subject (`MatchTarget`). A
`MatchNode` memoizes its Instance-shaped attribute map on first `view()`, one level deep: each
child capture becomes a lazy `Match` of its own.

That memo was per `MatchNode`, but `MatchNode`s were not per capture node. Every
materialization minted fresh child `MatchNode`s (new instance id, empty memo) from the same
`Arc<CapNode>`s, and only a per-materialization table kept two slots of *one* parent (an alias
`<val=word>`) on the same child (GH #8167). Two parents that hold the same node therefore
presented two different Matches, and each forced the shared subtree again.

A `.wrap`ped self-recursive token hits this at every level (#11404). Level *k*'s wrapper
receives its `callsame` result, a Match over the token's own captures, whose children are the
very `Arc<CapNode>`s the level above files under `$<TOP>`. Grammar::Extractor keeps each
level's result and renders it (`.raku`), so level *k* forced its whole subtree afresh: *N*²/2
materialized nodes for a recursion *N* deep, quadratic peak RSS (528 MB at depth 750), and
Grammar::Extractor's `t/01-basic` at 7.5x rakudo's wall time. In rakudo there is one Match
object per node, so `$parent<TOP>` *is* the Match the level below handed back.

A second per-level cost had the same shape: each wrapped call collected the engine's subject
into a fresh `String` and built a fresh `MatchTarget` (an `Arc<[char]>` copy) for the wrapper's
cursor, again for the token body, and once more to LTM-rank the rule's lone candidate.

## Decision

1. **A capture node has one canonical lazy Match per engine run.** `CapNode` carries a
   `MatchMemo`, a weak back edge (`WeakGc<MatchNode>`) to the Match presenting it.
   `Value::lazy_match(cap, target)` returns the memoized Match when it is still alive and was
   built against the same run (`MatchTarget::same_run`: the same subject buffer and the same
   cursor-class cell), and otherwise builds one and records it. Every slot and every parent
   that exposes the node, through any materialization, now hands out the same Match: the same
   instance id (`===`, `.WHICH`) and one memoized materialization. The per-materialization
   alias table is subsumed and removed.
2. **The back edge is weak.** A `MatchNode` holds its `CapNode` strongly; a strong edge back
   would be an `Arc` cycle the cycle collector does not see. A Match nobody holds is freed as
   before, and the next request builds a new one (with a new id, which nothing can observe
   because the old one is unreachable).
3. **A cloned node is a new node.** `MatchMemo::clone` is empty: the clone paths
   (`match_with_ast_lazy`, `with_match_cursor_pos`, `Arc::make_mut` in the reduce walk) clone
   precisely to present something different.
4. **A wrapped subrule call matches against the caller's subject.** The wrapper's cursor is
   built on the engine run's published `MatchTarget` (when the subrule's `chars` is it), and
   the terminal that `callsame` reaches runs the token body on the cursor's own target instead
   of re-collecting `.orig`. Every level of a wrapped recursion then shares one target, which
   is also what makes rule 1 apply across levels. A lone candidate of a rule with no proto is
   no longer LTM-measured before it is called (the measurement could not change the outcome).

5. **A Match forced under another cursor class is not reused.** A grammar parse stamps its
   class on the run only after the engine finished, so a Match that user code forced in
   mid-parse (a code block keeping `$<num>`, a wrapper keeping its `callsame` result) memoized
   a plain-`Match` attribute map while the run's cursors now report the grammar's class. Such a
   node reads as neither, and handing it to the action walk made `$/` a bare grammar instance
   (`make` died with `X::Make::MatchRequired`). `MatchNode::is_current` rejects it from the
   memo, so the finished tree builds a fresh Match for that node, as before this ADR.
   Since #12943 a grammar parse stamps its class from the start (`CursorClassScope`: a target
   built for the parse's own subject buffer is born stamped), so a mid-parse node of a grammar
   parse is current and shared; the check remains for any node forced before a later stamp.

6. **`.made` is mutable, shared state of the node** (amendment, 2026-10-12, #12954). In rakudo
   `make` writes the `.made` attribute of the one cursor object. mutsu's `make` and the post-parse
   action walk instead built a *copy* carrying the ast (`match_with_ast_keeping_id`,
   `match_with_ast_lazy`, an eager `make_instance` per walked node) and re-filed it in a rebuilt
   tree, so a Match kept in mid-parse never saw its `.made`. `CapNode` now carries a `MadeCell`
   (seeded by a reduce-time inline `{ make … }`), `make` writes it in place
   (`Value::set_match_ast_in_place`: the cell, mirrored into the memoized attribute map when the
   node is already forced; an eager Match writes its attribute map), and the action walk
   dispatches each node's action without rebuilding the tree. This is the one exception to "a
   Match is an immutable value": rule 1 makes "the node's Match" well defined, and the cell lives
   on the node rather than the `MatchNode` because the memo is weak — a `MatchNode` rebuilt for
   the node after the first one died must still read the `.made`. Like the node's other `Value`s,
   the cell is not traced by the cycle collector (a cycle routed through it defers, never
   corrupts). A cloned node copies the value the cell holds at the time of the clone.

## Rejected alternatives

- **A table owned by the `MatchTarget`, keyed by the `Arc<CapNode>` address.** It needs its
  own pruning (entries outlive the Matches they name) and a freed address can be reused by a
  new node unless the key also holds a `Weak<CapNode>`; the per-node weak edge has neither
  problem and dies with the node.
- **A strong memo on the node.** The `Arc` cycle above.
- **A `.made` cell on the `MatchNode`** (rule 6). The memo is weak, so once every holder of the
  `MatchNode` dropped it, the next `lazy_match` built a fresh one without the `.made`.
- **Rendering `.raku` straight from the capture node.** It would make a second
  implementation of the Match shape next to `materialize_map` (one primitive, one
  implementation), and it leaves the identity divergence in place.

## Consequences

- A node reached through two parents is materialized once while either parent's Match is
  alive. Grammar::Extractor's per-level results now share their subtrees: materializations are
  linear in the depth instead of quadratic.
- When no one keeps the per-level results, a wrapper that renders the whole subtree at every
  level still does quadratic work. That is inherent to the output (rakudo prints the same
  quadratic volume); only the constant factor differs.
- A wrapped token's `callsame` result is the caller's `$<rule>` (#12918): mutsu still files a
  capture node rebuilt from the token's captures, but when that node reads exactly as the
  returned Match's (`CapNode::presents_like`: same span and metadata, the very same child
  nodes), its memo is seeded with the returned Match (`Value::adopt_as_match_of`). An alias
  (`<x=rule>`) records the rule's name as `action_name` on the filed node, so it is not alike
  and gets its own Match.
