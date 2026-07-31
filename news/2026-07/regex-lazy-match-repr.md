# Lazy `Match` representation (ADR-0016 P5 repr swap)

The regex/grammar `Match` object is no longer built eagerly. A match now
produces `ValueRepr::Match(Gc<MatchNode>)` — a NaN-box kind holding the shared
subject string (`Arc<String>`) and the stored capture node (`Arc<CapNode>`,
which the matcher already built) plus a memoized `OnceLock<Gc<InstanceAttrs>>`.
The Instance-shaped attribute map materializes on first `view()` decode, ONE
level at a time: child captures become lazy `Match` values themselves, so a
subtree nobody observes never allocates anything.

Because `view()` presents a lazy Match as `ValueView::Instance("Match")`,
every unconverted consumer keeps working unchanged — the P5 seam
(`match_view.rs`) answers scalar reads (`.from`/`.to`/`.Str`/`.made`/`.orig`)
straight from the capture node without materializing, and the three
`make_match_object_*` builders now just synthesize a top-level `CapNode` from
the exploded capture axes.

The grammar-action walk gained two structural fast paths:

- an **actionless node** (no action method for its rule) is skipped without
  materializing anything — checked against the capture node via
  `match_walk_peek`;
- a **childless leaf WITH an action** (YAMLish's per-character
  `method space($/) { make ~$/ }` shape) runs its action with the lazy Match
  as `$/` and applies the `make` by building a fresh lazy node carrying the
  `ast` — no `InstanceAttrs` is ever created for it. The rule's reduce-time
  `:my $*x` bindings are re-installed around the action, same as the main
  walk (pinned by `t/grammar-per-match-dynvar-action.t`).

Keeping the leaf lazy across the action call required sweeping the dispatch
path's variant probes: ~20 sites that pattern-matched `value.view()` just to
ask "is this a Pair / Junction / Proxy / VarRef / ContainerRef / Seq / …?"
now use pure tag probes (`is_string_pair_value()` etc., one NaN-box page
compare) that cannot materialize a lazy Match — measured by instrumenting
`MatchNode` materialization on a YAMLish parse: leaf materializations fell
from 1807 (every leaf) to 15.

GC: `Gc<MatchNode>` is a collector node whose only traced edge is the
memoized materialization; the `Arc<CapNode>` payload is treated as externally
rooted per the shared-wrapper rule (conservative). `gc_trace` yields the node
handle via a tag probe so a collect can never trigger materialization.

Local interleaved A/B on an idle box (release, 5×5): `bench-yaml-parse`
1.31 s → 0.87 s (**≈ −34 %**). Authoritative numbers come from the bench CI
(`bench-history.tsv`) once merged, per the standing measurement policy.
